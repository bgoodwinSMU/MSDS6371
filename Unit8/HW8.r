######################
#    Ben Goodwin     #
#      MS6371        #
#       HW8          #    
######################


##################################################################
#Question 1


#read in bball dat
bbDat <- read.csv("teams.csv")

#looks good
#View(bbDat)

#plot wins and payroll
ggplot(data = bbDat, aes(x = Wins, y = Payroll)) + 
  geom_point(color='blue')

##################################################################


##################################################################
#Question 2

#compute correlation coefficient
wins = bbDat$Wins
payroll = bbDat$Payroll
cor(payroll,wins)

##################################################################

##################################################################
#Question 3

#same data minus SD
bbNoSD <- bbDat[-c(29),]

#looks good
#View(bbNoSD)

#plot wins and payroll
ggplot(data = bbNoSD, aes(x = Wins, y = Payroll)) + 
  geom_point(color='brown2')


#Check out correlation coefficient
winsNosd=bbNoSD$Wins
payrollNosd=bbNoSD$Payroll
cor(payrollNosd,winsNosd)
##################################################################


##################################################################
#Question 4

hist(log(bbDat$Payroll))
hist(log(bbDat$Wins))
bbDat$Wins <- as.factor(bbDat$Wins)
bbDat$Payroll <- as.factor(bbDat$Payroll)


bb.lm <- lm(Wins~Team, data = bbDat)
bb.av <- aov(bb.lm)
summary(bb.av)

tukey.test <- TukeyHSD(bb.av)
tukey.test


##################################################################

