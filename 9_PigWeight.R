# MArch 29th 2019
# Lecture 9

library(SemiPar)
data(pig.weights)
write.csv (pig.weights, "pig_weights.csv")

# package for graphing:
library(lattice)
xyplot(weight~num.weeks,
       data=pig.weights,
       groups=id.num,
       type="b")

# Fitting the linear Mixed model:
library(lmerTest)
f<- lmer(weight ~ num.weeks + (1|id.num), 
         data = pig.weights)
summary(f)

coef(f)

################
f <- lmer(weight ~ num.weeks + (1|id.num), 
          data = pig.weights)

f
# line 1: 17.67 = 19.36 - 1.68


####
library(mgcv)
ct1 <- gam(Volume )