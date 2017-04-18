# read in the data
setwd("z:/willem/teaching/envx2001")
Corn <- read.csv("2016_CornData_Topic9.csv")

# correlations
round(cor(Corn),3)

# cor.test
with(Corn,cor.test(CornP,InorgP))

# rcorr
#install.packages("Hmisc")
require(Hmisc,quiet=T)
rcorr(as.matrix(Corn))

# running MLR
MLR.Corn <- lm(CornP~InorgP + OrgP,data=Corn)
# run anova() to see the anova table
anova(MLR.Corn)
# run summary to see the parameter estimates
summary(MLR.Corn)

# check residuals
par(mfrow=c(2,2))
plot(MLR.Corn,which=c(1,2,5))
hist(resid(MLR.Corn))
par(mfrow=c(1,1))

# make a single prediction
# create a new data frame with the variables to predict at
# Note that it does not matter what you put in for CornP
new.df <- data.frame(CornP=0,InorgP=10,OrgP=42)
# now use predict() and specify se.fit=T
predCorn <- predict(MLR.Corn,newdata=new.df,se.fit=T)
# the output now has two elements:
# the fit
predCorn$fit
# the se of the fit
predCorn$se.fit

# now outside the original model:
# create a new data frame with the variables to predict at
# Note that it does not matter what you put in for CornP
new.df <- data.frame(CornP=rep(0,5),InorgP=26:30,OrgP=60:64)

Corn.predict <- predict(MLR.Corn,newdata=new.df,interval="prediction")
Corn.predict
# standard errors
predict.se <- (Corn.predict[,3] - Corn.predict[,1])/qt(0.975,14)
predict.se

# Exercise 2 Answers
Loyn <- read.csv("z:/willem/teaching/envx2001/2016_LoynData.csv")
names(Loyn)

# make histograms
par(mfrow=c(2,2))
hist(Loyn$ABUND)
hist(Loyn$AREA)
hist(Loyn$YR.ISOL)
par(mfrow=c(1,1))

# correlation and pairs plot
library(Hmisc)
rcorr(as.matrix(Loyn[,1:3]))
pairs(Loyn[,1:3])

# transformation
Loyn$L10AREA <- log10(Loyn$AREA)

# correlation and histogram again
par(mfrow=c(2,2))
hist(Loyn$ABUND)
hist(Loyn$L10AREA)
hist(Loyn$YR.ISOL)
par(mfrow=c(1,1))

rcorr(as.matrix(Loyn[,c(1,3,8)]))
pairs(Loyn[,c(1,3,8)])

# Exercise 3
Loyn2 <- read.csv("z:/willem/teaching/envx2001/2016_Loyn2.csv")
names(Loyn2)

# correlation
rcorr(as.matrix(Loyn2))

# do regression
lm.mod1 <- lm(ABUND~GRAZE + L10AREA,data=Loyn2)
summary(lm.mod1)
par(mfrow=c(2,2))
plot(lm.mod1,c(1,2,5))
hist(resid(lm.mod1))
par(mfrow=c(1,1))

# try transformed variable
lm.mod2 <- lm(ABUND~GRAZE + L10AREA + YR.ISOL,data=Loyn2)
summary(lm.mod2)
par(mfrow=c(2,2))
plot(lm.mod2,c(1,2,5))
hist(resid(lm.mod2))
par(mfrow=c(1,1))

# prediction
new.df <- data.frame(ABUND=0,GRAZE=2, L10AREA=1.6, YR.ISOL=1962)
pred.mod2 <- predict(lm.mod2,newdata=new.df,interval="prediction")
pred.mod2
