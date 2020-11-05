#Question 2

################################################################################
#Part I
#Read in data
autismDat <- read.csv("autismDat.csv")

#Check Data
#head(autismDat)

#Create LM for Prevalence vs Year
autlm <- lm(Prevalence ~ Year, data = autismDat)

#Extract year data
yearX <- autismDat$Year

#Sort year for plotting purposes
sortYear <- sort(yearX)

#Confidence Interval (95%)
prd_c <- predict(autlm, newdata= data.frame(Year = sortYear), interval=c("confidence"), 
                 type = c("response"), level=0.95)

#Prediction Interval (95%)
prd_p <- predict(autlm, newdata= data.frame(Year = sortYear), interval=c("prediction"),  level=0.95)

#Plot with confidence and prediction intervals
plot(autismDat[,1], autismDat[,2],xlim = c(1985,2005), ylim = c(0,40), xlab = "Year", 
     ylab = "Prevalence", main = "Autism and it's prevalence 1992-2000")


p <- abline(autlm, col = "darkseagreen4")+lines(sortYear,prd_c[,2],col = "blueviolet",lty = 2, lwd = 2)+lines(sortYear,prd_c[,3],col = "blueviolet", lty = 2, lwd = 2)+lines(sortYear,prd_p[,2],col = "deeppink2", lty = 2, lwd = 2)+lines(sortYear,prd_p[,3],col = "deeppink2", lty = 2, lwd = 2)
p


#Data def looks non-linear, log transform
autismDat$log.prevalence <- log(autismDat$Prevalence)


#Create LM for Prevalence vs Year
autlmLog <- lm(log.prevalence ~ Year, data = autismDat)



#Confidence Interval (95%)
prd_c <- predict(autlmLog, newdata= data.frame(Year = sortYear), interval=c("confidence"), 
                 type = c("response"), level=0.95)

#Prediction Interval (95%)
prd_p <- predict(autlmLog, newdata= data.frame(Year = sortYear), interval=c("prediction"),  level=0.95)

#Plot with confidence and prediction intervals
plot(autismDat[,1], autismDat[,2],xlim = c(1990,2000), ylim = c(0,3), xlab = "Year", 
     ylab = "Prevalence", main = "Autism and it's prevalence 1992-2000 \n (Log transformed)")


p <- abline(autlmLog, col = "darkseagreen4")+lines(sortYear,prd_c[,2],col = "blueviolet",lty = 2, lwd = 2)+lines(sortYear,prd_c[,3],col = "blueviolet", lty = 2, lwd = 2)+lines(sortYear,prd_p[,2],col = "deeppink2", lty = 2, lwd = 2)+lines(sortYear,prd_p[,3],col = "deeppink2", lty = 2, lwd = 2)
p

################################################################################
#Part II

#Residuals

#residuals for original model
autresid <- resid(autlm)
plot(autresid)




#residuals for log transforemd model

autresidlog <- resid(autlmLog)
plot(autresidlog)


################################################################################
#Part III


studentizedresaut <- rstudent(autlm)


hist(studentizedresaut)


xplot <- seq(min(studentizedresaut)-1, max(studentizedresaut)+1, length=40)


yplot <- (dnorm(xplot))


lines(xplot, yplot, ylim=c(0,0.5))


#Log transformed part

studentizedresautLog <- rstudent(autlmLog)


hist(studentizedresautLog)


xplotlog <- seq(min(studentizedresautLog)-1, max(studentizedresautLog)+1, length=40)


yplotlog <- (dnorm(xplotlog))


lines(xplotlog, yplotlog, ylim=c(0,0.5))


################################################################################
#Part IV

##Quantile-Quantile plots

par(mfrow=c(1,2))
p1 <- qqnorm(studentizedresaut)+qqline(studentizedresaut)
p2 <- qqnorm(studentizedresautLog)+qqline(studentizedresautLog)

################################################################################
#Part B

summary(autlmLog)

confint(autlmLog)


################################################################################
#Bonus 

steerDat <- read.csv("steerdat.csv")

steerDatAov <- aov(pH~as.factor(log.time),data = steerDat)
summary(steerDatAov)

steerlm <- lm(pH~log.time,data=steerDat)
summary(aov(steerlm))


