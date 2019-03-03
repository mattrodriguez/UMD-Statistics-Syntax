#===============================================================================
#
#  MRA 1: Review
#
#  Instructor: Jeff Harring
#
#  Date: 01/07/19
#
#===============================================================================

#Get the working directory
#Set the working directory

getwd()
setwd("C:/pathway name/")
setwd("C:\\pathway\\name\\")

setwd("C:\\Users\\harring\\Documents\\Teaching\\651-2019\\Data\\Week-1")

#Calling up the car package

library(car)
data(Prestige, package="car")
head(Prestige)
str(Prestige)

#Writing a data set to an external file on your computer

write.csv(Prestige, "c:/your pathway/Prestige.csv")
write.table(Prestige, "c:/your pathway/Prestige.csv")
?write.table

#Creating a conditional scatterplot using the 
#scatterplot() function in the car package
#This could also be accomplished using the plot() function

scatterplot(prestige~income|type, data=Prestige, boxplots=FALSE,
smooth=FALSE,reg.line=FALSE,id.n=0,col=c(463,562,60),pch=c(16,18,20))

#Creating a scatterplot matrix using the scatterplotMatrix() function

scatterplotMatrix(~prestige+education+income, span=0.6, 
data=Prestige,reg.line=lm, id.n=2)

Prestige$inc = Prestige$income^(1/3)

scatterplotMatrix(~prestige+education+inc, span=0.6, 
data=Prestige,reg.line=lm, id.n=2)


#Create a correlation matrix corresponding to the scatterplot matrix

with(Prestige, cor(cbind(prestige,education,income)))

#Logarithmic functions for transforming in R

log(10)
log10(10)
log2(10)
log(10, base=2)


#Seeing the effects of a logarithmic transformation using
#the Ornsteirn data set

par(mfrow=c(1,2))
with(Ornstein, plot(density(assets), xlab="assets", main="(a)"))
with(Ornstein, plot(density(log10(assets)), 
xlab="base-10 log of assets", main="(b)"))

par(mfrow=c(1,2))
scatterplot(infant.mortality~gdp, data=UN, xlab="GDP per Capita", 
ylab="Infant Mortality Rate (per 1000 births)", main="(a)", boxplot=FALSE)
scatterplot(infant.mortality~gdp, data=UN, xlab="GDP per Capita", 
ylab="Infant Mortality Rate (per 1000 births)", main="(b)", boxplot=FALSE,
log="xy", id.n=3)


#linear model with transformed response and predictor using the UN data set

summary(lm(log(infant.mortality)~log(gdp), data=UN))

#Produce inverse transformation plot using the invTranPlot() in the car package

par(mfrow=c(1,2))
invTranPlot(prestige~income, data=Prestige, lwd=2, xlab="Income")

plot(prestige~I(income^(1/3)), data=Prestige, xlab="Income^(1/3)")
abline(lm(prestige~I(income^(1/3)), data=Prestige))

summary(p1 <- with(Prestige, powerTransform(cbind(income, education))))

testTransform(p1, lambda=c(.33,1))

testTransform(p1, lambda=c(0,1))

scatterplotMatrix(~prestige+education+log2(income), span=0.7, 
data=Prestige,reg.line=lm, id.n=2)




#Read in Math Achievement data and EDA

library(car)

math <- read.csv(file.choose(),header=TRUE,sep=",",na.strings="-99")
# math <- read.csv("setwd("C:\\Users\\harring\\Documents\\Teaching\\651-2019\\Data\\Week-1\\School.csv",
# header=TRUE,sep=",",na.strings="-99")
math <- read.csv("School.csv",header=TRUE,sep=",",na.strings="-99")

head(math)
tail(math)
str(math)


scatterplotMatrix(~math+hsgpa+ses, span=0.6, data=math, 
reg.line=lm, id.n=2)

with(math, cor(cbind(math,hsgpa,ses)))


#Fitting the model

math.lm <- lm(math~hsgpa+ses, data=math)

names(math.lm)
summary(math.lm)

math.lm.0 <- lm(math~1, data=math)
anova(math.lm.0,math.lm)

#n=dim(math)
#extractAIC(math.lm)
#extractAIC(math.lm, k=log(n))


#Compute confidence intervals and standardized regression coefficients

summary(math.lm)[[4]][2,2]
confint(math.lm)

install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(math.lm)


#Demonstrating the computation of partial correlation using residuals

e1 <- lm(math~hsgpa, data=math)$residuals
e2 <- lm(ses~hsgpa, data=math)$residuals
summary(lm(e1~e2, data=math))
cor(e1,e2)

install.packages("ppcor", dependencies=TRUE)
library(ppcor)
pcor(cbind(math$math,math$hsgpa,math$ses))

install.packages("lmSupport", dependencies=TRUE)
library(lmSupport)
#Running a MR model with 4 predictors



math2.lm <- lm(math~hsgpa+ses+mot+iq, data=math)
summary(math2.lm)
confint(math2.lm)
lm.beta(math2.lm)
modelEffectSizes(math2.lm)


#Running a full and reduced model delta F test

math1.lm <- lm(math~hsgpa+ses, data=math)
summary(math1.lm)

math2.lm <- lm(math~hsgpa+ses+mot, data=math)

math3.lm <- lm(math~hsgpa+ses+mot+iq, data=math)

anova(math1.lm, math3.lm)


#New example using Bank.csv data

dat1<-read.csv(file.choose(), header=TRUE, sep=",")
names(dat1)<-c("id", "bsal", "gender", "time", "age", "csal", "educ", "work", "jobcat")
ndat1<-as.data.frame(cbind(dat1[,6],dat1[,2],dat1[,7]))
names(ndat1)<-c("csal","bsal","educ")

#To produce a scatterplot 

library(stats)
par(mfrow=c(1,1),oma=c(0,0,3,0),adj=0.5)
plot(ndat1$csal~ndat1$bsal, data=ndat1, 
xlim=c(2500,20000), ylim=c(0,60000), type="p",
bty="l", main = "Current vs. Beginning Salary", 
col=4, pch=20)

#To produce the scatterplot matrix

#Lots of graphical choices here 
pairs(ndat1, col="red", pch=18)

invTranPlot(csal~educ, data=ndat1)

ndat1$newed = ndat1$educ**5

pairs(ndat1, col="blue", pch=20)

#Run the multiple regression

bfit1 = lm(csal~ bsal, data=ndat1)
summary(bfit1)
names(bfit1)
bfit1$residuals
bfit1$coefficients

bfit2 = lm(csal~ bsal+newed, data=ndat1)
summary(bfit2)
names(bfit2)
bfit2$residuals
bfit2$coefficients

anova(bfit1, bfit2)

2*(1-pt(.072,247))

#Matt's code to find p-value
2 * (1 - pt(4.355,245))
