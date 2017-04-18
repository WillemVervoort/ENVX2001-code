# ENVX2001 MLR lecture

# -----------------------------------------
# Solar irradiance and global temperature
# original data
GISTEMP <- read.csv("otherdata/GISTEMP.csv")

jpeg("2016_globaltempanomaly.jpeg")
plot(TempAnom~Year,data=GISTEMP,xlab="Year",
     ylab="Global temperature anomaly (Deg C/100)",
     type="l", col="blue", lwd=2,cex.lab=1.2, cex.axis=1.2, font.lab=2)
lines(GISTEMP$Year,filter(GISTEMP$TempAnom,rep(0.1,10),method="convolution"),
      col="red",lty=2,lwd=2)
legend("topleft",c("GlobalTemp anom","10 yr Moving Average"), 
       lty=c(1,2),col=c("blue","red"),lwd=2)
dev.off()

ML <- read.csv("otherdata/Maunaloa.csv",na.string="-99.99")
ML.y <- aggregate(ML$Average,list(Year=ML$Year),mean,na.rm=T)

plot(x~Year,data=ML.y,ylab="CO2 in ppm at Mauna Loa",
     col="blue", lwd=2,cex.lab=1.2, cex.axis=1.2, font.lab=2)

Irrad <- read.table("otherdata/Irrad.dat",header=T,colClasses=c("character","numeric","numeric"))
head(Irrad)

Irrad[Irrad[,3]<0,3]<-NA
Irrad[,1] <- as.character(Irrad[,1])

temp <- paste(ifelse(as.numeric(substr(Irrad[,1],1,2)) < 77,"20","19"),
              substr(Irrad[,1],1,2),"-",substr(Irrad[,1],3,4),"-",
              substr(Irrad[,1],5,6),sep="")

Irrad$Date <- as.Date(temp)
plot(Irrad$Date,Irrad[,3])


Irrad.y <- aggregate(Irrad[,3],list(Year=substr(Irrad$Date,1,4)),mean,na.rm=T)

plot(x~Year,data=Irrad.y,ylab="TSI - Watts/sq meter",
     col="blue", lwd=2,cex.lab=1.2, cex.axis=1.2, font.lab=2,
     ylim=c(1365,1367))

# datasets aligned, starting 1977
Gtemp <- read.csv("otherdata/20140519_GISTEMPregrdata.csv")
names(Gtemp)

# SLR with irradiance
lm.irrad <- lm(TempAnom~Irrad,data=Gtemp)
# summary
summary(lm.irrad)
# multi linear
lm.MultiL <- lm(TempAnom~CO2 + Irrad,data=Gtemp)

summary(lm.MultiL)
plots and thanova(lm.MultiL)
# residual plots
par(mfrow=c(2,2))
plot(lm.MultiL,cex=1.5,lwd=2,which=c(1,2,5))
hist(residuals(lm.MultiL))
par(mfrow=c(1,1))

#-------------------------
# Loyn data
# ----------------
Loyn <- read.csv("2016_LoynData.csv")
names(Loyn)
# transformation (discussed later)
Loyn$L10AREA <- log10(Loyn$AREA)

M1 <- lm(ABUND~L10AREA,data=Loyn)
summary(M1)

M2 <- lm(ABUND~YR.ISOL,data=Loyn)
summary(M2)

M3 <- lm(ABUND~L10AREA + YR.ISOL,data=Loyn)
summary(M3)
anova(M3)
# variance covariance matrix
with(Loyn,cov(cbind(ABUND,L10AREA,YR.ISOL)))

# residual plots
par(mfrow=c(2,2))
plot(M3,which=c(1,2,5),
     cex=2,lwd=2)
hist(residuals(M3))
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(M3,which=c(1,2,5),
     cex=2,lwd=2)
hist(rstandard(M3))
par(mfrow=c(1,1))


# look at correlation and pairs plots
Loyn_sub <- with(Loyn,cbind(ABUND,AREA,LDIST,DIST))
pairs(Loyn_sub,cex=2,cex.axis=1.5,font.axis=2)
round(cor(Loyn_sub),2)

# transforming data
Loyn_sub <- data.frame(Loyn_sub)
Loyn_sub$L10AREA <- log10(Loyn_sub$AREA)
Loyn_sub$L10LDIST <- log10(Loyn_sub$LDIST)
Loyn_sub$L10DIST <- log10(Loyn_sub$DIST)

pairs(Loyn_sub[,c("ABUND","L10AREA","L10LDIST","L10DIST")],
      cex=2,cex.axis=1.5,font.axis=2)
round(cor(Loyn_sub[,c("ABUND","L10AREA","L10LDIST","L10DIST")]),2)


# Load new dataset (transformed) for MLR Loyn
Loyn2 <- read.csv("2016_Loyn2.csv")

#With all variables
MLR_total <- lm(ABUND~.,data=Loyn2)
#anova(MLR_total)
summary(MLR_total)

# drop L10LDIST
MLR_total2 <- lm(ABUND~YR.ISOL+GRAZE+ALT+L10DIST+L10AREA,data=Loyn2)
#anova(MLR_total2)
summary(MLR_total2)

# residual plots
par(mfrow=c(2,2))
plot(MLR_total2,cex=1.5,
     lwd=2,which=c(1,2,5))
hist(resid(MLR_total))
par(mfrow=c(1,1))

# Best regression?
MLR_total3 <- lm(ABUND~YR.ISOL+GRAZE+ALT+L10AREA,data=Loyn2)
anova(MLR_total3)
summary(MLR_total3)


MLR_total4 <- lm(ABUND~YR.ISOL+GRAZE+L10AREA,data=Loyn2)
anova(MLR_total4)
summary(MLR_total4)

# careful with the anova table
MLR_total4 <- lm(ABUND~YR.ISOL+GRAZE+L10AREA,data=Loyn2)
anova(MLR_total4)

MLR_total5 <- lm(ABUND~GRAZE+L10AREA+YR.ISOL,data=Loyn2)
anova(MLR_total5)

# check with partial F?
MLR_0 <- lm(ABUND~1,data=Loyn2)
MLR_1 <- lm(ABUND~YR.ISOL,data=Loyn2)
anova(MLR_0,MLR_1)
