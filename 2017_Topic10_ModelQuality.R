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
