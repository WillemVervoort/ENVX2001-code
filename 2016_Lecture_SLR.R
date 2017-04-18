# -------------------
# Matrices
# ------------------

Carbon <- c(48, 56, 90, 78, 86, 71, 42)

# following linear algebra from Bendix Carstensen (2015)
# matrix A
A <- matrix(c(1,3,2,2,8,9),ncol=3)
A
# multiply with a scalar
7*A

# matrix B
B <- matrix(c(5,8,3,4,2,7),ncol=3,byrow=T)
# addition
A + B
# multiplication
A%*%B

# Determinant
D <- matrix(c(5,3,9,6),2,2)
D
det(D)
# Inverse
solve(D)

# cannot find inverse if determinant = 0
E <- matrix(c(1,2,3,6),2,2)
E
det(E)
solve(E)

# ----------------------------
# Regression
# soil temperature data
# -----------------------------
ST <- read.csv("SoilEvaporation.csv")
head(ST)
plot(ST)

# Regression model formula is always y~x
SLR <- lm(SoilEvap_mm_per_day~MaxSoilT_C,data=ST)
anova(SLR)
summary(SLR)
# plotting
par(mfrow=c(2,2))
plot(SLR,1:2)
hist(residuals(SLR))
par(mfrow=c(1,1))

# ------------------
# matrices
# -----------
X <- cbind(rep(1,10),ST$MaxSoilT_C[1:10])
y <- ST$SoilEvap_mm_per_day[1:10]

XTX <- t(X)%*%X
XTX

XTy <- t(X)%*%y
XTy

invXTX <- solve(XTX)
invXTX

b = invXTX%*%XTy
b
# ---------------------
# Part 3
# Transformations
# ------------------
# alligators
alligator <- read.csv("alligator.csv")
plot(alligator)

# not transformed
notTrans <- lm(Weight~Length,data=alligator)
anova(notTrans)
summary(notTrans)
# plotting residuals
par(mfrow=c(2,2))
plot(notTrans,1:2)
hist(residuals(notTrans))
par(mfrow=c(1,1))
# transformed
Trans <- lm(log10(Weight)~Length,data=alligator)
anova(Trans)
summary(Trans)
# plotting residuals
par(mfrow=c(2,2))
plot(Trans,1:2)
hist(residuals(Trans))
par(mfrow=c(1,1))

# -----------------------
# Part 4
# make a prediction
# ------------------------
# repredict the data
predict(SLR,se.fit=T)


# predict for maxT from 35 - 40 C
# construct a prediction dataframe with same column names
pred.df<- data.frame(MaxSoilT_C=35:40,SoilEvap_mm_per_day=rep(NA,6))

# now use predict()
predict(SLR,newdata=pred.df,interval="prediction")

# predict with confidence values and plot
p <- predict(SLR, interval="confidence")
plot(ST, pch=16,cex=2,col="red")
lines(sort(ST[,1]),sort(p[,1]),lwd=2)
lines(sort(ST[,1]),sort(p[,2]),lty=2,lwd=2,col="blue")
lines(sort(ST[,1]),sort(p[,3]),lty=2,lwd=2,col="blue")

# -------------------
# Part 5
# Correlations
# --------------------
qqnorm(ST[,1], cex=2, pch=16, col="red",
       main="Maximum soil temperature")
qqline(ST[,1],lwd=2)

qqnorm(ST[,2], cex=2, pch=16, col="red",
       main="Soil Evaporation")
qqline(ST[,2],lwd=2)

# correlations
cor(ST)


