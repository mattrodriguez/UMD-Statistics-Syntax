#Get the working directory
#Set the working directory

getwd()
setwd("C:\\Users\\harring\\Documents\\Teaching\\651-2019\\Data\\Week-1")

Duncan <- read.table(file.choose(), 
header=T, sep=",", na.strings="-99")

#Calling up the car package and Duncan data set

#install.packages("car", dep=TRUE)
library(car)
data(Prestige, package="car")

head(Duncan)
tail(Duncan)
str(Duncan)

#Linear model fit and index plot for leverage values

mod.duncan1 <- lm(prestige~income+education,data=Duncan)
summary(mod.duncan1)

influenceIndexPlot(mod.duncan1, main="", vars=c("hat"), 
id = list(n=3))

#abline(h=0.09,lty=3,col="red",lwd=2),
#abline(h=0.13,lty=1,col="blue",lwd=2)


#infIndexPlot(model, vars=c("Cook", "Studentized", "Bonf", "hat"),
#id=TRUE, grid=TRUE, main="Diagnostic Plots", ...)


mod.duncan1 <- lm(prestige~income+education,data=Duncan)
mod.duncan2 <- lm(prestige~income,data=Duncan)

anova(mod.duncan2,mod.duncan1)



#Outlier test in the Car package

#For those of you with a little more time to play, here are some of the same
#diagnostics using ggplot( )  function to do some "cool" graphing

stud.resid<-rstudent(mod.duncan1)
stud.resid.df<-data.frame(prof=names(stud.resid),studres=stud.resid)
stud.resid.df

prof.in.resid.order<-stud.resid.df$prof[order(stud.resid.df$studres)]
prof.in.resid.order

stud.resid.df$prof<-factor(stud.resid.df$prof,levels=prof.in.resid.order)

stud.resid.df$hjust <- ifelse(stud.resid.df$studres > 0, 1.3, -0.3)

library(ggplot2)
ggplot(data=stud.resid.df, aes(x=prof, y=studres)) + 
  geom_bar(colour="black", fill="#56B4E9", width=.7, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Profession") + ylab("Studentized Residual") +
  ggtitle("Studentized Residuals by Profession") +
  scale_y_continuous(breaks=-10:10)

last_plot() + coord_flip() + theme_bw()



outlierTest(mod.duncan1)





#Linear model fit and index plot for Cook's distance values

mod.duncan <- lm(prestige~income+education, data=Duncan)
infIndexPlot(mod.duncan, main="", vars=c("Cook"), id=list(n=3)) 

#abline(h=0.5,lty=1, lwd=2, col="red"))




#Linear model fit, computation of dfbetas

dfbs.duncan <- dfbetas(mod.duncan)
head(dfbs.duncan)

#Scatterplot of dfbetas

plot(dfbs.duncan[, c("income","education")])
identify(dfbs.duncan[,"income"], dfbs.duncan[,"education"],
Duncan$X)




#Influence plot using the Car package

mod.duncan <- lm(prestige~income+education, data=Duncan)
influencePlot(mod.duncan, main="", id=list(n=3))




#A way to run to regressions with and without cases and compare 
#the coefficients

mod.dun <- update(mod.duncan,
           subset=!(Duncan$X %in% c("minister")))

compareCoefs(mod.duncan, mod.dun, se=TRUE)




#Index plot for all regression diagnostics

#library(car)
mod.duncan <- lm(prestige~income+education, data=Duncan)
infIndexPlot(mod.duncan, main="", 
vars=c("Cook", "Studentized","Bonf","hat"), 
id=list(n=4))



#Residual plots using the Car package 

scatterplotMatrix(~prestige+education+income, data=Duncan,
id=list(n=3))

pres.mod <- lm(prestige~education+income, data=Duncan)
residualPlots(pres.mod)



#Kolmolgorov-Smirnoff Test of normality

pr.out <- lm(prestige~education+income, data=Duncan)
qqPlot(pr.out, id.n=3)
ks.test(pr.out$residuals, y="pnorm")
hist(pr.out$residuals, col="red")



#VIF function in HH package to examine multicollinearity

install.packages("HH", dep=TRUE)
require(HH)
pres.mod <- lm(prestige~education+income, data=Duncan, x=TRUE)
summary(pres.mod)
vif(pres.mod)

sqrt(vif(pres.mod))

tol<-1/vif(pres.mod); tol


















