#1
x <- read.csv("C:/Users/willi/OneDrive/Desktop/activity.csv")
data <- as.Date(x$date)
steps <- x %>% group_by(date) %>% summarize(sumsteps = sum(steps,na.rm = TRUE))
hist(steps$sumsteps,ylim = c(0,35),xlab="steps")

av <- mean(steps$sumsteps)
med <- median(steps$sumsteps)
print(paste("mean:",av))
print(paste("median:",med))
##2

stepsbyinterval <- x %>% group_by(interval) %>% summarize(intervalsteps = mean(steps,na.rm = TRUE))
plot(stepsbyinterval$intervalsteps~stepsbyinterval$interval,col = "blue",xlab = "Time",ylab = "Average Steps",type = "l")

highest <- stepsbyinterval$interval[which.max(stepsbyinterval$intervalsteps)]
print(paste("The 5-minute Interval is: ",highest))

amount <- max(stepsbyinterval$intervalsteps)
print(paste("The max steps is: ", amount))

##3
num <- sum(is.na(x$steps))

fill_in_missing <- x  
for (i in 1:nrow(x)){
  if(is.na(x$steps[i])){
    fill_in_missing$steps[i]<- stepsbyinterval$intervalsteps[fill_in_missing$interval[i] == stepsbyinterval$interval]
  }
}

steps_by_day <- fill_in_missing %>% group_by(date) %>% summarize(sumsteps = sum(steps, na.rm = TRUE))
hist(steps_by_day$sumsteps,xlab = "steps",main = "Steps per Day Frequency")

mean_day <- mean(steps_by_day$sumsteps)
median_day <- median(steps_by_day$sumsteps)

print(paste("mean:",mean_day))
print(paste("median:",median_day))
Print("both the median and mean increased")
## 4

four <- fill_in_missing
weekend <- subset(four,weekdays(as.POSIXlt(date))=="Sunday" |weekdays(as.POSIXlt(date))=="Saturday" )
weekday <- subset(four,weekdays(as.POSIXlt(date)) != "Sunday" & weekdays(as.POSIXlt(date)) != "Saturday")

g <- ggplot (weekend, aes(interval, steps))
g <- g + geom_line()
m <- ggplot (weekday, aes(interval, steps))
m <- m + geom_line()
