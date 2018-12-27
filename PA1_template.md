---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

# baseDir will be prefixing all data accesses
baseDir <- "."

# create data sub-directory if necessary
dataDir <- file.path(baseDir, "data")
if(!file.exists(dataDir)) { dir.create(dataDir) }

zipFilePath <- file.path(dataDir, "activity.zip")
dateFilePath <- file.path(dataDir, "date_time_downloaded.txt")
# download original data if necessary (skip if exists already as it takes time and bandwith)
if(!file.exists(zipFilePath)) { 
  zipFileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file (zipFileUrl, zipFilePath, method="curl")
  DTDownloaded <- format(Sys.time(), "%Y-%b-%d %H:%M:%S")
  cat (DTDownloaded, file=dateFilePath)
} else {
  DTDownloaded <- scan(file=dateFilePath, what="character", sep="\n")
}

filePath <- file.path(dataDir, "activity.csv")
# unzip file if necessary
if(!file.exists(filePath)) { 
  unzip (zipFilePath, exdir=dataDir)
}

# read dataset and load data in R
dataset <- read.csv(filePath, header = TRUE) 

cat ("The dataset is located at", filePath, "and was downloaded on", DTDownloaded)


str(dataset)

dataset$date <- as.Date(dataset$date)

dataset$minute <- dataset$interval %% 100
dataset$hour <- dataset$interval %/% 100
dataset$elapsed <- dataset$hour * 60 + dataset$minute
# interval as a factor
dataset$sInterval <- as.factor(sprintf("%02d:%02d", dataset$hour, dataset$minute))


dataset$interval <- dataset$interval / 100
# create a table with number of steps per day
sumStepsPerDay <- aggregate(steps ~ date, data=dataset, FUN="sum", na.exclude=T)
meanStepsPerInterval <- aggregate(steps ~ sInterval, data=dataset, FUN="mean", na.exclude=T)
#sumStepsPerDay
#meanStepsPerInterval$steps


## What is mean total number of steps taken per day?

histogram(sumStepsPerDay$steps, breaks=10, main="Total number of steps per day", xlab="Steps per day")
dev.copy(png, "Histogram of total number of steps taken each day.png", width = 480, height = 480)
dev.off()


mean(sumStepsPerDay$steps, na.rm=TRUE)

median(sumStepsPerDay$steps, na.rm=TRUE)


## What is the average daily activity pattern?

xyplot(steps ~ sInterval, data=meanStepsPerInterval, type="l", grid=TRUE, ylab="Number of steps", xlab="5-min. intervals from midnight", main="Average number of steps by 5-minutes intervals")
dev.copy(png, "Average number of steps taken.png", width = 480, height = 480)
dev.off()

intv <- meanStepsPerInterval$sInterval[which.max(meanStepsPerInterval$steps)]

## Imputing missing values

misst <- sum(is.na(dataset$steps))
misst

sum(is.na(dataset$date))

sum(is.na(dataset$interval))

str(meanStepsPerInterval)

# replace missig values w
datasetNoMissing <- dataset
for(r in 1:nrow(datasetNoMissing)){
  if (is.na(datasetNoMissing$steps[r])) {
    repl <- meanStepsPerInterval$steps[meanStepsPerInterval$sInterval == datasetNoMissing$sInterval[r]];
    datasetNoMissing$steps[r] <- repl;
  }
}
# we verify it worked
sum(is.na(dataset$steps))



sum(is.na(datasetNoMissing$steps))

str(dataset$steps)

str(datasetNoMissing$steps)

sumStepsPerDayNoMissing <- aggregate(steps ~ date, data=datasetNoMissing, sum)
histogram(sumStepsPerDayNoMissing$steps, breaks=10, main="Total number of steps per day (missing estimated)", xlab="Steps per day")
dev.copy(png, "2 - Histogram of total number of steps taken each day.png", width = 480, height = 480)
dev.off()




mean(sumStepsPerDayNoMissing$steps, na.rm=TRUE)

median(sumStepsPerDayNoMissing$steps, na.rm=TRUE)

meanStepsPerIntervalNoMissing <- aggregate(steps ~ interval, data=dataset, FUN="mean", na.exclude=T)
xyplot(meanStepsPerIntervalNoMissing$steps ~ meanStepsPerIntervalNoMissing$interval,type="l", grid=T, ylab="Number of steps", xlab="5-min. intervals from midnight", main="Average number of steps by 5-minutes intervals, missing values estimated")
dev.copy(png, "2 - Average number of steps taken.png", width = 480, height = 480)
dev.off()

## Are there differences in activity patterns between weekdays and weekends?
datasetNoMissing$day <- "weekday"
datasetNoMissing$day[weekdays(as.Date(datasetNoMissing$date), abb=T) %in% c("Sat","Sun")] <- "weekend"

table(datasetNoMissing$day)

meanStepsPerIntervalNoMissingDay <- aggregate(steps ~ interval + day, data=datasetNoMissing, FUN="mean")
xyplot(steps ~ interval | day, data=meanStepsPerIntervalNoMissingDay, type="l", grid=T, layout=c(1,2), ylab="Number of steps", xlab="5-min. intervals from midnight", main="Average  5-min. activity intervals: Weekdays vs. Weekends")
dev.copy(png, "Weekday vs weekend.png", width = 480, height = 480)
dev.off()
