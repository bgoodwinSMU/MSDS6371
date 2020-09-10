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

dat3 <- log(dat1)
dat4 <- log(dat2)

t.test(dat1,dat2)




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
