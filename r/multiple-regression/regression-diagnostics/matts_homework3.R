#Set working directory
setwd("~/Dropbox/PhD/UMD/2019_Spring_Semester_UMD/EDMS_651/Estimation_Model-Based_Inference/Assignment")

#Read the file
bigmac <- read.csv("BigMac.csv",header=TRUE,sep=",",na.strings="-99")

# Quick look at data
summary(bigmac)
View(bigmac)
str(bigmac)
levels(bigmac$City)
contrasts(bigmac$City)

#######Question 1
#Scatterplot matrix of variables
library(car)
plot1 <- with(bigmac, scatterplotMatrix(~BigMac+TeachSal+WorkHrs+BusFare+Bread, id=list(n=3), variable.names("City")))


######## Question 2

#Fitting the model with logged variables - Except WorkHrs
log.fit <- lm(log(bigmac$BigMac)~WorkHrs+log(bigmac$BusFare)+log(bigmac$Bread)+log(bigmac$TeachSal))
summary(log.fit)

stargazer(log.fit, title="Fitted Model Adjusting for BigMac, BusFare, and Bread. WorkHrs was not adjusted.", digits=3, align=TRUE, keep.stat="f", style="all")

#Plotting the fitted logged model
plot.fitted <- with(bigmac, scatterplotMatrix(~log(bigmac$BigMac)+log(bigmac$TeachSal)+log(bigmac$WorkHrs)+log(bigmac$BusFare)+log(bigmac$Bread), id=list(n=3), variable.names("City")))

#Plotting the fitted logged model - Except WorkHrs
plot.fitted2 <- with(bigmac, scatterplotMatrix(~log(bigmac$BigMac)+log(bigmac$TeachSal)+WorkHrs+log(bigmac$BusFare)+log(bigmac$Bread), id=list(n=3), variable.names("City")))

#Diagnostic for Outliers (Studentized Resid.), Leverage (Hat), (Bonferroni), and (Cook's D)
log.bigmac <- log(bigmac$BigMac)
log.teachsal <- log(bigmac$TeachSal)
log.busfare <- log(bigmac$BusFare)
log.bread <- log(bigmac$Bread)

mod.2 <- lm(log.bigmac~log.teachsal+WorkHrs+log.busfare+log.bread, data=bigmac)
summary(mod.2)
infIndexPlot(mod.2, main="", 
             vars=c("Cook", "Studentized","Bonf","hat"), 
             id=list(n=4))

#Creating a Cook's D Hat graph to show cutoff, which is 3(5+1)/45 = .4
infIndexPlot(mod.2, main="", 
             vars=c("hat"), 
             id=list(n=4))
abline(h=0.4, lty=1)

#Looking at DFBETAS
dfbetas.bigmac <- dfbetas(mod.2)
dfbetas.bigmac
head(dfbetas.bigmac)

#Creating a Bubble Plot that is a scatterplot of discrepancy, leverage, and influence
influencePlot(mod.2, main="", id=list(n=3))

###### Question 7
#Rerun regression with Lagos and Mexico City removed.
#A way to run to regressions with and without cases and compare the coefficients
mod.2 <- lm(log.bigmac~log.teachsal+WorkHrs+log.busfare+log.bread, data=bigmac)
mod.3 <- update(mod.2,
                  subset=!(bigmac$Casenumbers %in% c("18", "24")))
summary(mod.3)

stargazer(mod.3, title="Fitted Model with Lagos and Mexico City Removed", digits=3, align=TRUE, keep.stat="f", style="all")

#Plotting the residuals
residualPlots(mod.3)

#Plotting the studentized residuals
qqPlot(mod.3, id.n=3)

#Kolmolgorov-Smirnoff Test of normality
ks.test(mod.3$residuals, y="pnorm")

#Creating a histogram of the residuals
hist(mod.3$residuals, col="blue")

######## Question 10
infIndexPlot(mod.3, main="", 
             vars=c("Cook", "Studentized","Bonf","hat"), 
             id=list(n=4))

influencePlot(mod.3, main="", id=list(n=3))

#Removing five Cities from the model
mod.2 <- lm(log.bigmac~log.teachsal+WorkHrs+log.busfare+log.bread, data=bigmac)
mod.4 <- update(mod.2,
                subset=!(bigmac$Casenumbers %in% c("18", "24", "22", "8", "38")))
summary(mod.4)

infIndexPlot(mod.4, main="", 
             vars=c("Cook", "Studentized","Bonf","hat"), 
             id=list(n=4))

influencePlot(mod.4, main="", id=list(n=3))

#Plotting the residuals
residualPlots(mod.4)

#Plotting the studentized residuals
qqPlot(mod.4, id.n=3)

#Kolmolgorov-Smirnoff Test of normality
ks.test(mod.4$residuals, y="pnorm")

#Creating a histogram of the residuals
hist(mod.4$residuals, col="blue")

#I still need to remove cities 8 and 34
mod.2 <- lm(log.bigmac~log.teachsal+WorkHrs+log.busfare+log.bread, data=bigmac)
mod.5 <- update(mod.2,
                subset=!(bigmac$Casenumbers %in% c("18", "24", "22", "8", "38", "7", "33")))
summary(mod.5)

infIndexPlot(mod.5, main="", 
             vars=c("Cook", "Studentized","Bonf","hat"), 
             id=list(n=4))

influencePlot(mod.5, main="", id=list(n=3))

#Plotting the residuals
residualPlots(mod.5)

#Plotting the studentized residuals
qqPlot(mod.5, id.n=3)

#Kolmolgorov-Smirnoff Test of normality
ks.test(mod.5$residuals, y="pnorm")

#Creating a histogram of the residuals
hist(mod.5$residuals, col="blue")

#Testing the omnibus ANOVA to see if all four predictors account for a significant proportion of variance in y
anova.bigmac <- lm(mod.5)
summary(anova.bigmac)


#Testing each predictor's influence on y, controlling for the other variables in the model.
#See An R Companion to Applied Regression, "4.3 Predictor Effect Plots"
library(effects)
library(lattice)
plot(predictorEffects(mod.5), confint=list(type="Scheffe"))

stargazer(mod.5, title="Fitted Model with Log Transformation of BigMac, TeachSal, BusFare, and Bread while excluding Mexico City, Lagos, Madrid, Copenhagen, Chicago, Sydney, and Rio de Janeiro.", digits=3, align=TRUE, keep.stat="f", style="all")


###### Junk
library(ggplot2)
library(ggthemes) # See http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-5-scatterplots.html
library(extrafont)
library(datasets)
library(ggrepel) # See https://stackoverflow.com/questions/15624656/label-points-in-geom-point
mod.1 <- lm(BigMac~TeachSal+WorkHrs+BusFare+Bread,data=bigmac)
plot1 <- ggplot(mod.1, aes(x=TeachSal+WorkHrs+BusFare+Bread, y=BigMac, label=bigmac$City)) +
  geom_point(shape = 2) +
  xlab("TeachSal, WorkHrs, BusFare, and Bread") + ylab("BigMac") +
  ggtitle("Scatterplot for Exploratory Analysis")
plot1
plot1 +
  geom_label_repel(aes(label=bigmac$City),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()

#with(bigmac, scatterplotMatrix(~BigMac+TeachSal+WorkHrs+BusFare+Bread, id=list(n=3), labels = City))

outlier.1 <- outlierTest(mod.1)
outlier.1
#It appears observation 25, Mexico City, is significant in the associated Bonferroni p-value of .000, which demonstrates the magnitude of this studentized residual is terribly unusual to observer in a sample of 45 observations.

infIndexPlot(mod.1, main="", vars=c("Cook"),
             id.n=3)
abline(h=0.5, lty=1, lwd=2, col="red")
#It appears that Lagos has more Cook's D than Mexico City

#Now I look at the dfbetas, which compares regression coefficients when the ith case is included versus when it is not included
mod.1.dfbs <- dfbetas(mod.1)
head(mod.1.dfbs)

#Creating a Bubble Plot of discrepancy, leverage, and influence
influencePlot(mod.1, main="", id.n = 3)

#Creating index plots of diagnostic statistics
infIndexPlot(mod.1, main="", vars=c("Cook", "Studentized", "Bonf", "hat"))

#Creating a standard residuals plot
mod.1 <- lm(BigMac~TeachSal+WorkHrs+BusFare+Bread,data=bigmac)
residualPlots(mod.1)

#Creating a Kolmogorov-Smirnov test to see if the residuals came from a normal distribution.
ks.test(mod.1$residuals, y="pnorm") #the p-value is .000 which indicates no, the residuals did not come from a normal distribution

#Finding the RSS - residual sum of squares
deviance(mod.1)

#Using a square root transformation
library(MASS)
library(rcompanion)
mod.transform <- sqrt(bigmac$BigMac)

#Getting the residuals
mod.residuals <- residuals(mod.1)
mod.residuals

#Getting the fitted beta coefficients
mod.fitted <- fitted(mod.1)
mod.fitted

summary(mod.1)
log10(7)
log2()
log2(exp(1))
log(exp(1))
log10(exp(1))
p1 <- powerTransform(bigmac$BusFare ~ 1, data=bigmac, family="bcPower")
summary(p1)

