#DS6371 R HW6

#libraries
library(mosaic)
library(Sleuth3)
library(pairwiseCI)
library(multcomp)
library(coin)
library(DunnettTests)

trellis.par.set(theme=col.mosaic()) # get a better color scheme for lattice
options(digits=3)

case0601$Handicap = relevel(case0601$Handicap, ref="Amputee")
summary(case0601)

favstats(Score ~ Handicap, data=case0601)

bwplot(Handicap ~ Score, data=case0601)

densityplot(~ Score, groups=Handicap, auto.key=TRUE, data=case0601)

anova(lm(Score ~ Handicap, data=case0601))

summary(lm(Score ~ Handicap, data=case0601))

model.tables(aov(Score ~ Handicap, data=case0601))

#Question 1
handiDat <- read.csv("hanicap.csv")

# Step 1: Make sure that group is a factor
handiDat$Handicap <- as.factor(handiDat$Handicap)

# Step 2: Perform ANOVA
res <- aov(formula = Score ~ Handicap, data = handiDat)

# Step 3: Perform post-hoc analysis
require(DescTools)
PostHocTest(res, method = "bonferroni")
summary(res)

pairwiseCI(Score~Handicap,data=handiDat)

plot(res)



#################
#  Question 2   #
#################

library(agricolae)
handicap <- handiDat
aovHandi <- aov(Score ~ Handicap, data = handicap)
handi.bonf <- LSD.test(aovHandi, 'Handicap', p.adj='bonferroni')
handi.lsd <- LSD.test(aovHandi, 'Handicap', p.adj='none')
handi.scheffe <- scheffe.test(aovHandi, 'Handicap')
handi.tukey <- HSD.test(aovHandi, 'Handicap')
handi.bonf$statistics[6]
handi.tukey$statistics[5]
handi.lsd$statistics[6]
handi.scheffe$statistics[7]


gout <- glht(aovHandi, mcp(Handicap = 'Dunnett'))
confint(gout)
summary(gout)
gout$linfct


#################
#  Question 3   #
#################

educDat <- read.csv("educDat3.csv")
educDat$Educ <- as.factor(educDat$Educ)
educDat$Educ = relevel(educDat$Educ, ref="12")
summary(educDat)

favstats(Income2005 ~ Educ, data=educDat)

bwplot(Income2005 ~ Educ, data=educDat)

densityplot(~ Income2005, groups=Educ, auto.key=TRUE, data=educDat)

anova(lm(log(Income2005) ~ Educ, data=educDat))

summary(lm(log(Income2005) ~ Educ, data=educDat))

model.tables(aov(log(Income2005) ~ Educ, data=educDat))

dat <- aov(log(Income2005) ~ Educ, data=educDat)
plot(dat)

#Step 2
#Tukey-Cramer
model<-aov(log(Income2005) ~ Educ, data = educDat)
out <- HSD.test(model,"Educ", group=TRUE,console=TRUE,
                main="Income 2005\nDifferent educational levels")
plot(out)


tukey.test <- TukeyHSD(model)
summary(tukey.test)



glht(model, linfct = mcp(Educ = "Dunnett"))
