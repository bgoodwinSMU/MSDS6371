mlbdat <- read.csv("mlbdagt.csv")

mlblm <- lm(wins~payroll,data=mlbdat)
summary(mlblm)
mean(mlbdat$payroll)
mean(mlbdat$wins)

mlblm$call
anova(mlblm)
resid(mlblm)

confint(mlblm)


new.payroll <- data.frame(payroll = c(100))


predict(mlblm, newdata = new.payroll,interval = "confidence",level = 0.95)

predict(mlblm, newdata = new.payroll, interval = "prediction",level = 0.95)

testdat <- read.csv("testdat.csv")

testlm <- lm(math~science,data = testdat)
summary(testlm)

confint(testlm,level = 0.99)
