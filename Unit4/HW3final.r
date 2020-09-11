##DS6371 HW3
##Ben Goodwin


#########################
#                       #
#      Question 1B      #
#                       #
#########################

##Libraries
library(dplyr)
library(car)
library(coin)
library(ggplot2)
library(lmPerm)

fired <- c(34,37,37,38,41,42,43,44,44,45,45,45,46,48,49,53,53,54,54,55,56,NA,NA,NA,NA,NA,NA,NA,NA,NA)
notFired <- c(27,33,36,37,38,38,39,42,42,43,43,44,44,44,45,45,45,45,46,46,47,47,48,48,49,49,51,51,52,54)

#t-test
t.test(fired,notFired)

firedCSV <- read.csv("firedStatus.csv")


#this is a non-parametric test which is very robust against departures from normality.
#For all these tests, the null hypothesis is that all populations variances are equal; 
#the alternative hypothesis is that at least two of them differ.
fligner.test(fired~notFired,data=firedCSV)


#Permutation test 
independence_test(fired ~ notFired,data = firedCSV)

#lmPerm permutation test
summary(lmp(fired~notFired,data=firedCSV))


##QQ for Fired
qqnorm(fired, pch = 1, frame = FALSE)
qqline(fired, col = "steelblue", lwd = 2,xlab="Fired data")

##QQ for NotFired
qqnorm(fired, pch = 1, frame = FALSE)
qqline(notFired, col = "steelblue", lwd = 2)

##Hist for fired
g = fired
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=2, breaks=10, prob=TRUE, 
     xlab="x-variable", ylim=c(0, 0.1), 
     main="Normal curve over fired data")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Hist for notFired
g = notFired
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=2, breaks=10, prob=TRUE, 
     xlab="Not Fired", ylim=c(0, 0.1), 
     main="Normal curve over not fired data")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


#########################
#                       #
#      Question 2       #
#                       #
#########################

cashDat <- read.csv("cash.csv")

#this is a non-parametric test which is very robust against departures from normality.
#For all these tests, the null hypothesis is that all populations variances are equal; 
#the alternative hypothesis is that at least two of them differ.
fligner.test(smu~seattleU,data=cashDat)

##QQ for SMU
qqnorm(cashDat$smu, pch = 1, frame = FALSE)
qqline(cashDat$smu, col = "steelblue", lwd = 2,xlab="SMU data")

##QQ for SeattleU
qqnorm(cashDat$seattleU, pch = 1, frame = FALSE)
qqline(cashDat$seattleU, col = "steelblue", lwd = 2,xlab="Seattle U data")

##Hist for SMU
g = cashDat$smu
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=1, breaks=20, prob=TRUE, 
     xlab="Cash in Pocket", ylim=c(0, 0.02), 
     main="Normal curve over SMU data")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Hist for Seattle U
h = cashDat$seattleU
m1<-mean(h)
std1<-sqrt(var(h))
hist(h, density=2, breaks=10, prob=TRUE, 
     xlab="Cash in Pocket", ylim=c(0, .1), 
     main="Normal curve over Seattle U data")
curve(dnorm(x, mean=m1, sd=std1), col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Permutation test 
independence_test(smu ~ seattleU,data = cashDat)

#lmPerm permutation test
summary(lmp(smu ~ seattleU,data = cashDat))

#ttest for CI
SMU = c(34, 1200, 23, 50, 60, 50, 0, 0, 30, 89, 0, 300, 400, 20, 10, 0)
Seattle = c(20, 10, 5, 0, 30, 50, 0, 100, 110, 0, 40, 10, 3, 0)
t.test(SMU,Seattle)


##Data with outlier removed

cashNoOutlier <- read.csv("cashNoOutlier.csv")

##QQ for SMU
qqnorm(cashNoOutlier$smu, pch = 1, frame = FALSE)
qqline(cashNoOutlier$smu, col = "steelblue", lwd = 2,xlab="SMU data")

##QQ for SeattleU
qqnorm(cashNoOutlier$seattleU, pch = 1, frame = FALSE)
qqline(cashNoOutlier$seattleU, col = "steelblue", lwd = 2,xlab="Seattle U data")

##Hist for SMU
g = cashNoOutlier$smu
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=1, breaks=20, prob=TRUE, 
     xlab="Cash in Pocket", ylim=c(0, 0.1), 
     main="Normal curve over SMU data")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Hist for Seattle U
h = cashNoOutlier$seattleU
m1<-mean(h)
std1<-sqrt(var(h))
hist(h, density=2, breaks=10, prob=TRUE, 
     xlab="Cash in Pocket", ylim=c(0, .1), 
     main="Normal curve over Seattle U data")
curve(dnorm(x, mean=m1, sd=std1), col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Permutation test 
independence_test(smu ~ seattleU,data = cashNoOutlier)

#lmPerm permutation test
summary(lmp(smu ~ seattleU,data = cashNoOutlier))

