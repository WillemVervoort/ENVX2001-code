# ENVX2001 Variable Selection Topic 9

Loyn2 <- read.csv("2016_Loyn2.csv")
names(Loyn2)

# slide 6
# Best regression?
MLR_total3 <- lm(ABUND~YR.ISOL+GRAZE+ALT+L10AREA,data=Loyn2)
anova(MLR_total3)
summary(MLR_total3)


MLR_total4 <- lm(ABUND~YR.ISOL+GRAZE+L10AREA,data=Loyn2)
anova(MLR_total4)
summary(MLR_total4)


# slide 19
# REDUCED
Mod1 <- lm(ABUND~ GRAZE + L10AREA + YR.ISOL,data=Loyn2)
anova(Mod1)
summary(Mod1)

#FULL
Mod2 <- lm(ABUND~ GRAZE + L10AREA + YR.ISOL + L10DIST, 
           data=Loyn2)
anova(Mod2)
summary(Mod2)


qf(.95,1,51)
# [1] 4.030393

# partial F-test
anova(Mod1,Mod2)

# Demonstrating step()
# backward
# define the maximum model
maxMod <- lm(ABUND~.,data=Loyn2) # include all variables
# run step, this automatically runs backward
step(maxMod)


# going forward
# define both minimum and maximum model
minMod <- lm(ABUND~1,data=Loyn2) # no variables
maxMod <- lm(ABUND~.,data=Loyn2) # all variables
step(minMod,scope=formula(maxMod),direction="forward")
