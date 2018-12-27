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