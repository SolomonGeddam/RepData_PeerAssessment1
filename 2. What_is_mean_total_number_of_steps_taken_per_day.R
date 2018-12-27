histogram(sumStepsPerDay$steps, breaks=10, main="Total number of steps per day", xlab="Steps per day")
dev.copy(png, "Histogram of total number of steps taken each day.png", width = 480, height = 480)
dev.off()


mean(sumStepsPerDay$steps, na.rm=TRUE)

median(sumStepsPerDay$steps, na.rm=TRUE)