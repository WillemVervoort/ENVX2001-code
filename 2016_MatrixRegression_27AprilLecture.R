## ENVX 2001 27 April

# MATRIX REGRESSION
# We want to find:

# (XTX)^-1 XTY = b

# load the data
SoilE <- read.csv("SoilEvaporation.csv")

# only use the first 10 rows
SoilE_red <- SoilE[1:10,]

X <- cbind(rep(1,10),SoilE_red$MaxSoilT_C)
Y <- SoilE_red$SoilEvap_mm_per_day

# t(X)%*%X
XT <- t(X)
XTX <- XT%*%X
invXTX <- solve(XTX)

# XTY
XTY <- XT%*%Y

# find b
b <- invXTX%*%XTY
b
