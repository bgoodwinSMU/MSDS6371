#HW 1, question 4
SMU = c(34, 1200, 23, 50, 60, 50, 0, 0, 30, 89, 0, 300, 400, 20, 10, 0)
Seattle = c(20, 10, 5, 0, 30, 50, 0, 100, 110, 0, 40, 10, 3, 0)
hist(SMU)
hist(Seattle)

data <-read.csv("SchoolMoney.csv")


data$school <- as.factor(data$school)
t.test(data$amount ~ data$school)

library(lmPerm)
summary(lmp(amount~school,data=data))



#Live session questions
#Slide 9, CI question
data1 <- c(25,19,37,29,40,28,31)
xbar <-mean(data1)
me<-2.447*(7.08/sqrt(7))
xbar-me
xbar+me
sd(data1)
t <- ((xbar-21)/(sd(data1)/sqrt(7)))
t
pt(t,6)

#checking
t.test(x=data1, mu=21, conf.level=0.95)
2*pt(3.309315, 6, lower=FALSE)


#Slide 13
#2 sample t test
t.test(SMU, Seattle)
sd(SMU)
sd(Seattle)
