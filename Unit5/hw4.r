######################
#    Ben Goodwin     #
#        HW4 R       #
######################


##Libraries
library(dplyr)


######################
#                    #
#    Question 2      #
#                    #
######################


logDat <- read.csv("logs1.csv")
#head(logDat)
logDat$Action <- as.numeric(logDat$Action)
wilcox.test(logDat$Action,logDat$PercentLost, correct=FALSE)




######################
#                    #
#    Question 3      #
#                    #
######################

educationDat <- read.csv("educationDat.csv")
head(educationDat)
dat1 <- filter(educationDat, Educ == 12)
dat2 <- filter(educationDat, Educ == 16)
t.test(dat1,dat2,var.equal = TRUE)

#Transform original data
dat3 <- log(dat1)
dat4 <- log(dat2)

#t-test on original data
t.test(dat1,dat2)

#t-test on transformed data
t.test(dat3,dat4)




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


##Hist for dat1
g = dat1$Income2005
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=2, breaks=10, prob=TRUE, 
     xlab="x-variable", ylim=c(0, 0.00003), 
     main="Normal curve 12 year education non transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Hist for dat2
g = dat2$Income2005
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=2, breaks=10, prob=TRUE, 
     xlab="Not Fired", ylim=c(0, 0.00003), 
     main="Normal curve 16 year education non transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Hist for dat3
g = dat3$Income2005
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=2, breaks=10, prob=TRUE, 
     xlab="x-variable", ylim=c(0, 0.00003), 
     main="Normal curve 12 year education transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Hist for dat4
g = dat4$Income2005
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=2, breaks=10, prob=TRUE, 
     xlab="Not Fired", ylim=c(0, 0.00003), 
     main="Normal curve 16 year education transformed")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")




