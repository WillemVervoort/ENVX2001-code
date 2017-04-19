# set working directory
setwd("z:/willem/teaching/envx2001")

# soil evaporation
SoilE <- read.csv("otherdata/SoilEvaporation.csv")


plot(SoilE$MaxSoilT_C, SoilE$SoilEvap_mm_per_day,xlab="Maximum daily soil T (Celcius)",
     ylab="Soil evaporation (mm/day)",
     # graphical parameters to make plot look pretty
     # increase the size of the symbols by 50%
     cex=1.5,
     # double line width and make colour red
     lwd=2,col="red",
     # increase size of fonts on labels and axis (30 and 20%)
     cex.lab=1.3, cex.axis=1.2)

# fit a model
SLR <- lm(SoilEvap_mm_per_day ~ MaxSoilT_C, data=SoilE)
summary(SLR)
# show the line of best fit, line double width
lines(SoilE$MaxSoilT_C,predict(SLR),lwd=2)
# add line of the mean soil evap
lines(sort(SoilE$MaxSoilT_C),rep(mean(SoilE$SoilEvap_mm_per_day),nrow(SoilE)),
      # "type = 2" (dashed), colour blue and width double
      lty=2,col="blue",lwd=2)


hist(SoilE$MaxSoilT_C)
hist(SoilE$SoilEvap_mm_per_day)


# # Only first 10 values
# SoilE.2 <- SoilE[1:10,]
# plot(SoilE.2[,1], SoilE.2[,2],xlab="Maximum daily soil T (Celcius)",
# ylab="Soil evaporation (mm/day)",cex=1.5,lwd=2,col="red",cex.lab=1.3,
# cex.axis=1.2)
# 
# # fit a model
# line <- lm(SoilE.2[,2] ~ SoilE.2[,1])
# summary(line)
# lines(SoilE.2[,1],predict(line),lwd=2,col="green")
# lines(sort(SoilE.2[,1]),rep(mean(SoilE.2[,2]),nrow(SoilE.2)),lty=2,col="blue",lwd=2)

## predictions
# Create a prediction data frame between 35 - 40 C and no data for soil evap
pred.df <- data.frame(MaxSoilT_C = 35:40, SoilEvap_mm_per_day = rep(NA,6))
# now use predict on SLR again
newEvap <- as.data.frame(predict(SLR,newdata=pred.df, interval="prediction",level=0.95))
newEvap

# We can also get the confidence intervals for the predictions within the model fit:
p <- as.data.frame(predict(SLR, interval="confidence"))
head(p)



# replot the model, but extend the axes
plot(SoilE$MaxSoilT_C, SoilE$SoilEvap_mm_per_day,xlab="Maximum daily soil T (Celcius)",
     ylab="Soil evaporation (mm/day)",
     # graphical parameters to make plot look pretty
     # increase the size of the symbols by 50%
     cex=1.5,
     # double line width and make colour red
     lwd=2,col="red",
     # increase size of fonts on labels and axis (30 and 20%)
     cex.lab=1.3, cex.axis=1.2,
     # extend the axes
     xlim=c(20,45), ylim=c(0,80))

# show the line of best fit, line double width
lines(SoilE$MaxSoilT_C,predict(SLR),lwd=2)
# add the confindence intervals
lines(sort(SoilE$MaxSoilT_C),sort(p$lwr),lty=2,lwd=2,col="blue")
lines(sort(SoilE$MaxSoilT_C),sort(p$upr),lty=2,lwd=2,col="blue")

# add the new points
points(pred.df$MaxSoilT_C,newEvap$fit, col="purple", lwd=2,cex=1.5)
# add the lwr and upr 
lines(sort(pred.df$MaxSoilT_C),newEvap$lwr,lty=2,lwd=2,col="green")
lines(sort(pred.df$MaxSoilT_C),newEvap$upr,lty=2,lwd=2,col="green")


# We can also get the prediction intervals for the model data:
p2 <- as.data.frame(predict(SLR, interval="prediction",level=0.95))
head(p2)

# replot the model (original axis)
plot(SoilE$MaxSoilT_C, SoilE$SoilEvap_mm_per_day,xlab="Maximum daily soil T (Celcius)",
     ylab="Soil evaporation (mm/day)",
     # graphical parameters to make plot look pretty
     # increase the size of the symbols by 50%
     cex=1.5,
     # double line width and make colour red
     lwd=2,col="red",
     # increase size of fonts on labels and axis (30 and 20%)
     cex.lab=1.3, cex.axis=1.2)

# show the line of best fit, line double width
lines(SoilE$MaxSoilT_C,predict(SLR),lwd=2)
# add the confindence intervals
lines(sort(SoilE$MaxSoilT_C),sort(p$lwr),lty=2,lwd=2,col="blue")
lines(sort(SoilE$MaxSoilT_C),sort(p$upr),lty=2,lwd=2,col="blue")
lines(sort(SoilE$MaxSoilT_C),sort(p2$lwr),lty=2,lwd=2,col="green")
lines(sort(SoilE$MaxSoilT_C),sort(p2$upr),lty=2,lwd=2,col="green")
legend("topleft",c("data","best fit","confidence", "prediction se"),
       pch=c(1,NA,NA,NA),lty=c(NA,1,2,2),col=c("red",1,"blue","green"))


# Back to the Loyn data set
Loyn <- read.csv("otherdata/2017_Loyn2.csv")
names(Loyn)

set.seed(2)

#Sample 20% of the rows, find row numbers
indexes <- sample(1:nrow(Loyn), size = 0.2*nrow(Loyn))
#Split data
valid <- Loyn[indexes,]
dim(valid)  # 11 7
calib <- Loyn[-indexes,]
dim(calib) # 45 7

##SUMMARY STATISTICS - RESPONSE 
# check validation and calibration data set
par(mfrow=c(2,2))

boxplot(calib$ABUND,ylab="Abundance forest birds",main="Calibration")
boxplot(valid$ABUND,ylab="Abundance forest birds",main="Validation")

boxplot(calib$L10AREA,ylab="Patch Area (ha)",main="Calibration")
boxplot(valid$L10AREA,ylab="Patch Area (ha)",main="Validation")

boxplot(calib$YR.ISOL,ylab="Year isolated",main="Calibration")
boxplot(valid$YR.ISOL,ylab="Year isolated",main="Validation")

boxplot(calib$L10DIST,ylab="Distance to nearest patch (km)",main="Calibration")
boxplot(valid$L10DIST,ylab="Distance to nearest patch (km)",main="Validation")

boxplot(calib$L10LDIST,ylab="Distance to nearest larger patch (km)",main="Calibration")
boxplot(valid$L10LDIST,ylab="Distance to nearest larger patch (km)",main="Validation")

boxplot(calib$GRAZE,ylab="Grazing intensity",main="Calibration")
boxplot(valid$GRAZE,ylab="Grazing intensity",main="Validation")

boxplot(calib$ALT,ylab="Elevation (m)",main="Calibration")
boxplot(valid$ALT,ylab="Elevation (m)",main="Validation")

par(mfrow=c(1,1))

# use stepwise backwards
maxMod <- lm(ABUND ~ ., data = calib) # include all variables
# run step, this automatically runs backward
step(maxMod)


# use model 2 and model 3 from topic 9 practical (last week)
# and test which one is the best model, but use calib data
ML.Loyn <- lm(ABUND ~ YR.ISOL + GRAZE + L10AREA, data = calib)

summary(ML.Loyn)

# Now check RMSE and bias

#Accuracy (RMSE)
# Calibrations
(RMSE <- sqrt(mean((calib$ABUND - predict(ML.Loyn))^2)))

# now validations
(RMSE_valid <- sqrt(mean((valid$ABUND - predict(ML.Loyn, newdata = valid))^2)))

#Bias
(Bias <- mean(calib$ABUND - predict(ML.Loyn)))
# very small, as they should be!

(Bias_valid <- mean(valid$ABUND - predict(ML.Loyn, newdata = valid)))
# much larger

# plot predicted versus observed
par(mar=c(5,5,4,2))
plot(calib$ABUND, predict(ML.Loyn), pch = 16, col = "red", cex = 2,
     xlab = "Observed dataset ABUND", ylab = "Predicted ABUND",
     cex.lab = 1.5, cex.axis = 1.5)
abline(0, 1, lty = 2, lwd = 2)
points(valid$ABUND, predict(ML.Loyn, newdata = valid), 
       col = "blue", pch = 16, cex = 2)
legend("topleft", c("calibration", "validation", "1 : 1 line"), 
       pch = c(16, 16, NA), lty = c(NA, NA, 2), col = c("red", "blue", 1), cex=1.3)

#Correlation
cor(calib$ABUND, predict(ML.Loyn))
# we already know this from summary()

cor(valid$ABUND, predict(ML.Loyn, newdata = valid))
# confirms bias and RMSE calculations

#Lin's
#install.packages("epiR")

library(epiR)

calib.lcc<-epi.ccc(calib$ABUND, predict(ML.Loyn))

calib.lcc$rho.c

valid.lcc<-epi.ccc(valid$ABUND, predict(ML.Loyn, newdata = valid))

valid.lcc$rho.c
