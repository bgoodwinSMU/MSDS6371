#####################
#   Homework 10     #
#    DS6371         #
#####################

#Question 1
#libraries
library(ggplot2)

#read data
birdDat <- read.csv("birdDat.csv")
head(birdDat)

#EDA the data
plot(birdDat$Mass,birdDat$Tcell)


#make da regression model
birdLm <- lm(Tcell~Mass,data = birdDat)
summary(birdLm)

plot(birdDat$Mass,birdDat$Tcell , ylim=c(0, 0.7), xlab="Mass", ylab="Tcell", main="Regression, CI, PI")
abline(birdLm, col="lightblue")
newx <- seq(3, 10, by=0.05)
conf_interval <- predict(birdLm, newdata=data.frame(Mass=newx), interval="confidence",
                         level = 0.99)
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)



pred_interval <- predict(birdLm, newdata=data.frame(Mass=newx), interval="prediction",
                         level = 0.99)
lines(newx, pred_interval[,2], col="orange", lty=2)
lines(newx, pred_interval[,3], col="orange", lty=2)




#Question C

qt(0.01, df=19, lower.tail=FALSE)

confint(birdLm,level=0.99)
new.speeds <- data.frame(Mass = c(4.5))
predict(birdLm, newdata = new.speeds)

#Question G/H

new.dat <- data.frame(Mass=4.5)
predict(birdLm, newdata = new.dat, interval = 'confidence', level = .99)
predict(birdLm, newdata = new.dat, interval = 'prediction', level = .99)

#Calibration 

(cal <- calibrate(birdLm, y0 = 3, interval = "inversion"))

