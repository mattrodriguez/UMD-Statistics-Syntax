#Set working directory
setwd("~/Dropbox/PhD/UMD/2019_Spring_Semester_UMD/EDMS_651/MRA_Review_I/Assignments")

########## Question 1
library(tidyverse)
library(tibble)
# x = from 1 to 100
# n = sample size = 150
# replace = true means no element occurs twice
temp_wind <- tibble(wind = sample(1:200, 1000, replace = TRUE), temp = (wind-100) ^2)
qplot(x = wind, y = jitter(temp, amount = 400)/100, data = temp_wind) + 
  labs( 
    x = "Wind Speed in Miles Per Hour", 
    y = "Temperature of Day in Fahrenheit"
    )

# Save the plot above
ggsave("question1.png")

########## Questions 2
# Read in fuel90.csv data
fuel <- read.csv(file.choose(),header=TRUE,sep=",",na.strings="-99")
fuel <- read.csv("Fuel90.csv",header=TRUE,sep=",",na.strings="-99")

# Quick look at data
summary(fuel)

# View all of the data
View(fuel)

# Simple linear regression
# y response variable = fuel
# x1 predictor variable = inc
fuel.mod <-lm(fuel ~ inc, data=fuel)

# Outputting the simple linear regression: 
# residuals, coefficients, residual SE, R^2, adjusted R^2
# F-statistic, DF, and p-value
summary(fuel.mod)

# Creating an ANOVA Table
fuel.anova <- anova(fuel.mod)
fuel.anova

# Create a basic plot of the data; y first, x second
with(fuel, plot(fuel ~ inc))

#save plot as a .png file
png('question2a.png')
with(fuel, plot(fuel ~ inc))
dev.off()

# Generating a nicely formatted regression table so I can copy/paste into Overleaf
library(stargazer)
stargazer(fuel.mod, title="Unstandardized Prediction Regression", digits=3, align=TRUE)

# Generate a formatted ANOVA Table
stargazer(fuel.anova, title="ANOVA", digits=3, align=TRUE, font.size = "small")

# Finding the critical value of the DF
abs(qt(0.05/2, 49))

# Find the predicted value for 15,000
fit = 916.871 + (-19.947*15)
fit

###########Question 3
# Find p-value of ANOVA
1-pf(27.039,5,169)
?pf
pf(27.039, 5, 169)
1-pf(27.039, 5, 169, digits=5)

# Given values 
n <- 175 
p <- 5 
ms_reg <- 60 
ss_resid <- 375  

# Calculated values 
df_reg <- p 
ss_reg <- ms_reg * df_reg 
df_resid <- n-p-1 
ms_resid <- ss_resid/df_resid  
ss_tot <- ss_resid + ss_reg 
f <- ms_reg/ms_resid 
p_value <- pf(f, df_reg, df_resid, lower.tail = F)

############# Question 4
# Kathleen's Kabel table package
# works with Knitr package
# crimecor<- kable(with(Crime, cor(cbind(crime, pctmetro, pctwhite, poverty, single))))

crime <- read.csv(file.choose(),header=TRUE,sep=",",na.strings="-99")
crime <- read.csv("crime.csv",header=TRUE,sep=",",na.strings="-99")

# Quick look at data
summary(crime)

# View all of the data
View(crime)

# Multiple linear regression
# y response variable = crime
# x1 predictor variable = pctmetro
# x2 predictor variable = pctwhite
# x3 predictor variable = poverty
# x4 predictor variable = single
crime.mod <-lm(crime ~ pctmetro + pctwhite + poverty + single, data=crime)

# Outputting the multiple linear regression: 
# residuals, coefficients, residual SE, R^2, adjusted R^2
# F-statistic, DF, and p-value
summary(crime.mod)

# Generating a nicely formatted regression table so I can copy/paste into Overleaf
library(stargazer)
stargazer(crime.mod, title="Unstandardized Prediction Regression", digits=3, align=TRUE)

# Creating a correlation matrix
crime.cor <- with(crime, cor(cbind(crime, pctmetro, pctwhite, poverty, single)))
crime.cor

# Outputting a fancy correlation table
stargazer(crime.cor, title="Correlation Matrix", digits=3, align=TRUE)

# Creating an ANOVA Table
crime.anova <- anova(crime.mod)
crime.anova

# Generate a formatted ANOVA Table with very tight space to reduce width
stargazer(crime.anova, title="ANOVA", digits=3, align=TRUE, font.size = "small", no.space=TRUE, column.sep.width = "-5pt")

# Comparing Model 1 (crime is being predicated by no other variable 
# so the predication is the mean of crime) and 
# Model 2 (The mean of crime vs. Crime with all variables)
crime.mod.0 <-lm(crime ~ 1, data=crime)
crime.anova.compare <- anova(crime.mod.0, crime.mod)
crime.anova.compare

# Outputting fancy ANOVA chart
library(stargazer)
stargazer(crime.anova.compare, title="ANOVA", digits=3, align=TRUE, font.size = "small", no.space=TRUE, column.sep.width = "-5pt")

# Create a MR table [with p-values] instead of standard errors for the predictors
library(stargazer)
stargazer(crime.mod, title="Unstandardized Prediction Regression", digits=3, align=TRUE, report=('vc*p'))

# Performing a delta F-Test
crime.lm1 <- lm(crime ~ pctmetro+poverty+single, data=crime)
crime.lm2 <- lm(crime ~ pctmetro+poverty+single+pctwhite, data=crime)
crime.anova.delta <- anova(crime.lm1, crime.lm2)
crime.anova.delta

# Outputting to a fancy ANOVA table
library(stargazer)
stargazer(crime.anova.delta, title="ANOVA", digits=3, align=TRUE, font.size = "small", no.space=TRUE, column.sep.width = "-5pt")

# Re-run MR [without pctwhite] and compute squared-part correlation for each of 3 predictors
crime.modelwithout <-lm(crime ~ pctmetro + poverty + single, data=crime)
summary(crime.modelwithout)
stargazer(crime.modelwithout, title="Regression Without Percentage of White", digits=3, align=TRUE)

# View all installed packages in R Studio
installed.packages()

# Installing the ppcor package and including dependencies
install.packages("ppcor", dependencies=TRUE)

# Computing the squared-part correlation for each of the 3 predictors in MR model
library(ppcor)
crime.part <-pcor(cbind(crime$crime, crime$pctmetro, crime$poverty, crime$single))
crime.part

# Create pretty table of the squared-part correlation table
stargazer(crime.part, title="Squared-Partial Correlations", digits=3, align=TRUE)

################ Extra Credit

