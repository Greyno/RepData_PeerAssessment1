#Reproducible Research
#Week 2: Peer Assessment 1 
getwd()
library(graphics)
library(ggplot2)
library(lattice)
#-------------------
#Loading and preprocessing the data
#Load in the dataset
activity <- read.csv("activity.csv")
summary(activity) #2304 NA values
head(activity)
tail(activity)
class(activity)
#Check to see if the data need to be converted/processed
class(activity$date) #The dates are a factor
class(activity$interval) #The intervals are an integer
class(activity$steps) #The steps are an integer

#Convert the date factors to dates
activity$date <- as.Date(activity$date, format = "%Y-%m-%d") 
#Convert the intervals to factors
activity$interval <- as.factor(activity$interval)
class(activity$date) #The dates are now a date
class(activity$interval) #The intervals are now in a factor format
str(activity)

#----------------------
#What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
#First sort the data into each day (have two months of data taken in Oct and Nov 2012)
#Use aggregate with the 'sum' function to find the total number of steps each day
activity_by_day<-aggregate(activity["steps"], by=activity["date"], FUN=sum)
head(activity_by_day)
tail(activity_by_day)
str(activity_by_day)
summary(activity_by_day) #Min 41 steps, max 21194 steps, 8 NAs

#Create a histogram (graphics pagkace) of the total number of steps taken each day
#Look at a histogram of the frequency by steps
hist(activity_by_day$steps, main=paste("Histogram of Numer of Steps/day (NA values removed)"), xlab="Number of steps per day", ylab="Frequency of steps")
#Calculate the mean and median of the number of steps/day (ignore NA values)
#Put (report) these values into a sentence when I do the RMarkdown
#mean (NAs removed):10766.19
mean_steps<-round(mean_steps<-mean(activity_by_day$steps, na.rm=TRUE), digits=2) 
#median (NAs removed):10765
median_steps<-round(median(activity_by_day$steps, na.rm=TRUE))

#---------
#What is the average daily activity pattern?
#Need to sort the data into the 5-minute intervals (x-axis) and the average number of steps taken, averaged across all days (y-axis)
activity_by_interval<-aggregate(activity["steps"], by=activity["interval"], FUN=mean, na.rm=TRUE) 
#Convert the intervals from a factor to an integer else 'type=l' will be ignored since the plot function will default to plot.factor, which does not take type. 
#activity_by_interval$interval <-as.integer(activity_by_interval$interval)
str(activity_by_interval)
plot(as.integer(activity_by_interval$interval), activity_by_interval$steps, type="l", ylab="Number of steps", xlab="Interval", main="Avg activity pattern")
head(activity_by_interval)
str(activity_by_interval)
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_step <- max(activity_by_interval$steps) #The max step is 206
max_interval <-round(which.max(activity_by_interval$steps))
head(max_interval) #The max steps occurs at the 104th interval value (need to convert back to time interval)

#----------
#Imputing missing values
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

missing_values <- round(sum(is.na(activity$steps))) #There are 2304 NAs in dataset
head(activity) #This is the original dataset
replaced_data<-activity #Make a copy of the original dataset
#Replace the NAs in the original data with the activity_by_interval data
for (i in 1:nrow(replaced_data)) {
    if (is.na(replaced_data$steps[i])) {
        replaced_data$steps[i] <- activity_by_interval[which(replaced_data$interval[i] == activity_by_interval$interval), ]$steps
    }
}
head(replaced_data)
missing_values <- round(sum(is.na(replaced_data$steps))) #There are no NAs in this new dataset
#Plot the data with the missing values replaced
replaced_data_activity_by_day<-aggregate(replaced_data["steps"], by=replaced_data["date"], FUN=sum)
head(replaced_data_activity_by_day)
hist(replaced_data_activity_by_day$steps, main=paste("Histogram of Numer of Steps/day (missing values replaced)"), xlab="Number of steps per day", ylab="Frequency of steps")

#Calculate the mean and median of the number of steps/day (NA values replaced)
#Put (report) these values into a sentence when I do the RMarkdown
#mean (NAs replaced): 10766.19
mean_steps_replaced<-round(mean_steps_replaced<-mean(replaced_data_activity_by_day$steps), digits=2)
#median (NAs replaced): 10766
median_steps_replaced<-round(median(replaced_data_activity_by_day$steps))

#-----
#Are there differences in activity patterns between weekdays and weekends?
#Use the dataset with the filled in data (i.e. use replaced_data_activity_by_day)
head(replaced_data_activity_by_day)
tail(replaced_data_activity_by_day)
str(replaced_data_activity_by_day)
#Use the weekdays() function to create a column with the days of the week appended to the dataset.
replaced_data_activity_by_day$weekday<-weekdays(replaced_data_activity_by_day$date)
data_by_days<-replaced_data_activity_by_day
head(data_by_days)
tail(data_by_days)
#test$weekday <- as.factor(test$weekday) # weekdays
#Split the data by days
weekend_data <- subset(test, weekday %in% c("Saturday","Sunday"))
weekday_data <- subset(test, !weekday %in% c("Saturday","Sunday"))
head(weekend_data)
#Replace "Sat" and "Sun" with "weekend"
weekend_data$weekday <- ifelse(weekend_data$weekday > "Friday","weekend", "")
#Replace "Mon" etc with "weekday"
head(weekday_data)
weekday_data$weekday <- ifelse(weekday_data$weekday >="Monday","weekday", "weekday")
head(weekday_data)
#Recombine the weekend and weekday data with rbind (this appends weekdays to the end)
data_by_days_final<-rbind(weekend_data, weekday_data)
tail(data_by_days_final)
head(data_by_days_final)
test_final$interval<-aggregate(test_final["steps"], by=test_final["date"], FUN=mean) 
head(test_final)
tail(test_final)
str(test_final)
#Plot the final dataset by weekend and weekday
#xyplot(test_final ~ interval.steps | weekday, data=test_final.interval.steps, layout=c(1,1))

ggplot(data_by_days_final, aes(x=date, y=steps)) + geom_line(color="black") + facet_wrap(~ weekday, nrow=2, ncol=1) + labs(x="Interval", y="Number of steps") + theme_bw()


#ggplot(data_by_days_final, aes(x=date, y=interval.steps)) + geom_line(color="black") + facet_wrap(~ weekday, nrow=2, ncol=1) + labs(x="Interval", y="Number of steps") + theme_bw()
