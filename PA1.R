library(dplyr)
library(lubridate)
library(lattice)

## Download the data in .zip format, unzip the file, and read it in 

dataurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile <- "activity.zip"
datafile <- "activity.csv"
download.file(dataurl, zipfile)
unzip(zipfile, datafile)
activity <- read.csv(datafile)

## Look at the summary of the activity
summary(activity)

## Convert dates and times to a single date-time in the same time zone as me.
allintervals <- as.integer(names(table(activity$interval)))
hours <- floor(allintervals/100)
mins <- allintervals - hours * 100
hoursmins <- paste(hours, mins, sep = ":")
head(hm(hoursmins))
timeints <- as.POSIXct(hm(hoursmins), origin = "1970-01-01", tz = "GMT")
intconvert <- data.frame(interval=allintervals, timeint=as.POSIXct(timeints))

##activitymod <- transmute(activity, steps = steps, timeint = timeint)


## What is the mean number of steps taken each day?
days <- group_by(activity, date) %>% 
    summarize(tot.steps.per.day = sum(steps, na.rm = TRUE))
head(days)
summary(days)


## Make a histogram (and show difference to barplot)
hist(days$tot.steps.per.day, 
     breaks = 10, 
     xlab = "Steps per day", 
     main = "Histogram of steps taken per day")
barplot(days$tot.steps.per.day,
        xlab = "Steps per day",
        names.arg = days$date,
        main = "Barplot of steps taken per day")

## Get mean and median of steps taken each day
summary(days$tot.steps.per.day)
mean.tot.steps.per.day <- mean(days$tot.steps.per.day)
median.tot.steps.per.day <- median(days$tot.steps.per.day)

## What is the average daily activity pattern?

intervals <- group_by(activity, interval) %>%
    summarize(mean.steps.per.int = mean(steps, na.rm = TRUE))

## Look at the summary of the intervals
intervalsmod <- merge(intervals,intconvert)
plot(intervalsmod$timeint, intervalsmod$mean.steps.per.int, type = "l")


## Which 5-minute interval, on average across all the days in the dataset
## contains the maximum number of steps?
maxsteps <- max(intervals$mean.steps.per.int)
whichmax <- which(intervals$mean.steps.per.int == maxsteps)
maxint <- intervals[whichmax,1][[1]]

## Imputed values

sum(is.na(activity$steps))/ length(activity$steps)


imputedactivity <- activity
imputedactivity <- merge(activity, intervals)

nasteps <- is.na(imputedactivity$steps)
imputedactivity$steps[nasteps] <- 
    imputedactivity$mean.steps.per.int[nasteps]
imputedactivity <- select(imputedactivity, -mean.steps.per.int)

daysimputed <- group_by(imputedactivity, date) %>% 
    summarize(tot.steps.per.day = sum(steps, na.rm = TRUE))
head(daysimputed)
summary(daysimputed)

summary(daysimputed$tot.steps.per.day)
mean.tot.steps.per.day.imp <- mean(daysimputed$tot.steps.per.day)
median.tot.steps.per.day.imp <- median(daysimputed$tot.steps.per.day)

hist(daysimputed$tot.steps.per.day, 
     breaks = 10, 
     xlab = "Steps per day", 
     main = "Histogram of steps taken per day (imputed)",
     ylim = c(0,25))

hist(days$tot.steps.per.day, 
     breaks = 10, 
     xlab = "Steps per day", 
     main = "Histogram of steps taken per day",
     ylim = c(0,25))


daysdiff <- merge(days, daysimputed, by = "date")



