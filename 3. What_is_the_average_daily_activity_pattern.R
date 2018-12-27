xyplot(steps ~ sInterval, data=meanStepsPerInterval, type="l", grid=TRUE, ylab="Number of steps", xlab="5-min. intervals from midnight", main="Average number of steps by 5-minutes intervals")
dev.copy(png, "Average number of steps taken.png", width = 480, height = 480)
dev.off()

intv <- meanStepsPerInterval$sInterval[which.max(meanStepsPerInterval$steps)]