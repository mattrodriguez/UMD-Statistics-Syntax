#===============================================================================
#
#  MRA 2: Predictors
#
#  Instructor: Jeff Harring
#
#  Date: 01/28/19
#
#===============================================================================

#Get the working directory
#Set the working directory

getwd()
#setwd("C:\\Users\\harring\\Documents\\Teaching\\651-2019\\Data")

# This is for the Prestige.csv file in week 1
Prestige <- read.csv("Prestige.csv",header=TRUE,sep=",",na.strings="-99")

#Calling up the car package

library(car)
data(Prestige, package="car")
head(Prestige)

#Checking how R is reading the variable "type"

Prestige$type
class(Prestige$type)

#Reordering the levels of a factor

Prestige$type <- with(Prestige, factor(type, levels=c("bc","wc", "prof")))
select <- c(1,2,35,36,61,62)
Prestige$type[select]

#Coercing a factor to have numeric values

type.number <- as.numeric(Prestige$type)
type.number[select]

#Seeing how R treats a factor coding by default

w <- factor(rep(c("a","b","c","d"), c(6,3,7,4))); w
model.matrix(~w)
contrasts(w)

#Reading in the Bank data

bank <- read.csv("bank.csv", header=TRUE, sep=",", na.strings=-99)
head(bank)
class(bank$gender)
#Setting the levels and labels of gender variable
#Not for the Prestige data, but in general

bank$sex <- factor(bank$gender, levels=c(0,1), labels=c("Male","Female"))

t.test(csalary~sex, data=bank, var.equal = TRUE)

#Treating the factor with numeric values in regression

bank.out <- lm(csalary~gender, data=bank)
summary(bank.out)

#Treating the factor without numeric values 

model.matrix(~bank$sex)
contrasts(bank$sex)

bankfac.out <- lm(csalary~sex, data=bank)
summary(bankfac.out)

#####################
#Different coding schemes 
#In R there are several different
#coding schemes that are built in
#They are:
#contr.helmert
#contr.poly
#contr.sum
#contr.treatment
#contr.SAS
#####################

contrasts(bank$sex) = contr.sum(2)
contrasts(bank$sex)

bankeff.out <- lm(csalary~sex, data=bank)
summary(bankeff.out)







#Reading the Baumann Data
#ANOVA with 3 Levels

Baumann <- read.csv("baumann.csv", header=TRUE, sep=",", na.strings=-99)

library(car)

data(Baumann, package="car")
head(Baumann)
some(Baumann)
xtabs(~ group, data=Baumann)
with(Baumann, tapply(post.test.3, group, mean))
with(Baumann, tapply(post.test.3, group, var))


class(Baumann$group)
model.matrix(~Baumann$group)
contrasts(Baumann$group)

#Running the regression model 

mod.1 <- lm(post.test.3~group, data=Baumann)
summary(mod.1)

#Changing the reference group

summary(update(mod.1, . ~ . - group + relevel(group, ref="Strat")))







#Quantitative and qualitative predictors

bank$educ <- bank$educlvl - mean(bank$educlvl)

bank$gen <-factor(bank$gender)

model.matrix(~bank$gen)
contrasts(bank$gen)

#Gives the gender and education interaaction
bank.2 <- lm(csalary~gen*educ, data=bank)
summary(bank.2)

bank.out.2 <- update(bank.2, . ~ . - educ)
summary(bank.out.2)

#Adjusted means using the effects library

library(effects)
effect("gen", bank.2)
plot(effect("gen", bank.2))

mean(bank$educlvl[bank$gen==0])
mean(bank$educlvl[bank$gen==1])


#Another example with the prestige data with 2 quantitative predictors
#and a qualitative predictors 

library(car)
pres.1 <- lm(prestige~education+log2(income)+type, data=Prestige)
summary(pres.1)

library(effects)
effect("type", pres.1)
plot(effect("type", pres.1))
plot(allEffects(pres.1))





#Section 4.2: Interactions
#School.csv dataset

library(foreign)
math <- read.spss(file.choose(), use.value.labels = TRUE, to.data.frame = TRUE) 

library(effects)
math <- read.csv(file.choose(), header=T, sep=",", na.string=-99)


sch.out <- lm(math~ses*iq, data=math)
summary(sch.out)
effect("ses*iq", sch.out)
plot(effect("ses:iq", sch.out))
plot(allEffects(sch.out))

#For effects of simple slope
vcov(sch.out)


#Plotting the interaction effect

x = seq(40,160,10)
y1 = 11.73+0.009*x
y2 = 13.96+0.093*x
y3 = 16.19+0.177*x

plot(x,y1,type="n",bty="l",
xlab="IQ",ylab="Math Achievement Score",
ylim=c(5,45))
lines(x,y1,lty=1,lwd=2)
lines(x,y2,lty=3,lwd=2)
lines(x,y3,lty=6,lwd=2)




#Run linear regression for Moore data

library(car)
some(Moore)

Moore$fcategory <- factor(Moore$fcategory, levels=c("low","medium","high"))
Moore$partner.status <- relevel(Moore$partner.status, ref="low")

xtabs(~fcategory + partner.status, data=Moore)
with(Moore, tapply(conformity, 
list("Authoritarianism"=fcategory, 
"Partner's Status"=partner.status), mean))

moore.out <- aov(conformity ~ fcategory*partner.status, 
data=Moore)
summary(moore.out)

moore.out <- lm(conformity ~ fcategory*partner.status, 
data=Moore)
summary(moore.out)
cbind(Estimate=coef(moore.out), confint(moore.out))
anova(moore.out)

#Interaction plot

with(Moore, {
interaction.plot(fcategory, partner.status, conformity, type="b",
pch=c(1,16), cex=2, ylim=range(conformity), leg.bty="o")})

#Get options from R about what type of contrasts are available

getOption("contrasts")
with(Moore, contrasts(fcategory))

Moore$fcategory1 <- relevel(Moore$fcategory, ref="high")
with(Moore, contrasts(fcategory1))









#User-defined contrasts

#Reading the Baumann Data
#ANOVA with 3 Levels

library(car)

data(Baumann, package="car")
head(Baumann)
some(Baumann)
xtabs(~ group, data=Baumann)
with(Baumann, tapply(post.test.3, group, mean))
with(Baumann, tapply(post.test.3, group, var))


class(Baumann$group)
model.matrix(~Baumann$group)
contrasts(Baumann$group)

#Running the regression model 

mod.1 <- lm(post.test.3~group, data=Baumann)
summary(mod.1)

#Changing the reference group

summary(update(mod.1, . ~ . - group + relevel(group, ref="Strat")))


L <- matrix(c(1,-0.5,-0.5, 0,1,-1),3,2)
colnames(L) <- c("Basal vs. DRTA & Strat", "DRTA vs. Strat")
L

contrasts(Baumann$group) <- L
Bau.out <- lm(post.test.3 ~ group, data=Baumann)
summary(Bau.out)























