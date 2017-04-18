# checking se.fit in R from predict.lm()
setwd("z:/willem/teaching/envx2001")
Corn <- read.csv("2016_CornData_Topic9.csv")

SLR.Corn <- lm(CornP~InorgP,data=Corn)
s.C <- summary(SLR.Corn)
a.C <- anova(SLR.Corn)
x <- Corn$InorgP

Sxx <- function(x) sum((x-mean(x))^2)

se.out <- function(MSE=a.C$`Mean Sq`[2],n=nrow(Corn),x) {
    sqrt(MSE*(1+1/n+((x-mean(x))^2)/Sxx(x)))
}
se.in <- function(MSE=a.C$`Mean Sq`[2],n=nrow(Corn),x) {
  sqrt(MSE*(1/n+((x-mean(x))^2)/Sxx(x)))
}

se.calc <- se.in(x=Corn$InorgP)
se.calc2 <- se.out(x=Corn$InorgP)
test <- predict(SLR.Corn,se.fit=T)

se.calc2
test2 <- predict(SLR.Corn,interval="confidence",level=0.99)
(se.calc2-se.calc)/sqrt(MSE)


