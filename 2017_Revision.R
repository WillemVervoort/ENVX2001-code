# ENVX2001 Revision lecture 2017

# demo difference between F-test
# In the lecture in week 9 (topic 9) we showed this
Loyn2 <- read.csv("2017_Loyn2.csv")
names(Loyn2)
# slide 11
# REDUCED
Mod1 <- lm(ABUND~ GRAZE + L10AREA + YR.ISOL,data=Loyn2)
summary(Mod1)

#FULL
Mod2 <- lm(ABUND~ GRAZE + L10AREA + YR.ISOL + L10DIST, 
           data=Loyn2)
summary(Mod2)


qf(.95,1,51)
# [1] 4.030393

# partial F-test
anova(Mod1,Mod2)

# Why can we do this?
# t^2 = F?

# what about this model
Mod3 <- lm(ABUND ~ L10AREA,data=Loyn2)
summary(Mod3)

# is this better than MOD1?
anova(Mod1,Mod3)
# p-values? why not

# and why can I not compare this model to Mod1?
Mod4 <- lm(ABUND ~ L10LDIST + L10AREA,data=Loyn2)
summary(Mod4)
# why is this not valid?
anova(Mod1,Mod4)


# this is where step() comes in
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
