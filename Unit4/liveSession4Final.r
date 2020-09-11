#DS6371 Live Assignment 4



#Libraries
library(BSDA)
library(dplyr)

#########################
#                       #
#      Question 1       #
#                       #
#########################


newMethod <- c(37,49,55,77)
oldMethod <- c(23,31,46)

#Try Wilcox rank sum test
wilcox.test(newMethod,oldMethod,alternative = "two.sided")

#Try Sign test
SIGN.test(newMethod,OldMethod = NULL, md=0, alternative = "two.sided",conf.level=0.95)

#T-test

tTest <- t.test(newMethod,oldMethod)
plot(newMethod~oldMethod)



#########################
#                       #
#      Question 2       #
#                       #
#########################


educationDat <- read.csv("educationDat.csv")
head(educationDat)
dat1 <- filter(educationDat, Educ == 12)
dat2 <- filter(educationDat, Educ == 16)

mean(dat1$Income2005)
mean(dat2$Income2005)
sd(dat1$Income2005)
sd(dat2$Income2005)

dat3 <- log(dat1)
dat4 <- log(dat2)

t.test(dat1,dat2,var.equal = TRUE)


##Hist for dat1
g = dat1$Income2005
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=1, breaks=20, prob=TRUE, 
     xlab="Income", ylim=c(0, 0.00002), 
     main="Normal curve over 12 year education dat")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Hist for dat2
h = dat2$Income2005
m1<-mean(h)
std1<-sqrt(var(h))
hist(h, density=2, breaks=10, prob=TRUE, 
     xlab="Income", ylim=c(0, 0.00001), 
     main="Normal curve over 16 year education dat")
curve(dnorm(x, mean=m1, sd=std1), col="darkblue", lwd=2, add=TRUE, yaxt="n")

##QQ for dat1
qqnorm(dat1$Income2005, pch = 1, frame = FALSE)
qqline(dat1$Income2005, col = "steelblue", lwd = 2)


##QQ for dat2
qqnorm(dat2$Income2005, pch = 1, frame = FALSE)
qqline(dat2$Income2005, col = "steelblue", lwd = 2)

##QQ for dat3
qqnorm(dat3$Income2005, pch = 1, frame = FALSE)
qqline(dat3$Income2005, col = "steelblue", lwd = 2)

##QQ for dat4
qqnorm(dat4$Income2005, pch = 1, frame = FALSE)
qqline(dat4$Income2005, col = "steelblue", lwd = 2)

#Permutation test 
independence_test(smu ~ seattleU,data = cashNoOutlier)

#lmPerm permutation test
summary(lmp(smu ~ seattleU,data = cashNoOutlier))
