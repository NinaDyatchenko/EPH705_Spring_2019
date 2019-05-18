### Leture 8. March 22d 2019
setwd("C:/Users/lxw391/Box Sync/TEACHING/2019-LW-lectures")

################### exact logistic regression

complete <- read.csv ("8_data_exactLogist.csv")
complete

# wrong:
lr <- glm (response ~ gender + region, 
           weights = count, 
           family = binomial, data = complete)
summary(lr)
# sSE is too big

# Firth method
library(logistf)
logistf (response ~ gender + region, weights = count, 
         family = binomial, 
         data = complete)


################### matched pairs
x <- matrix(c (20, 10, 5, 10), 2, 2)
dimnames (x) <- list(hus_rep = c("yes", "no"), wif_rep = c("yes", "no") )

x
#  gives wrong result
mcnemar.test(x)

mcnemar.test(x, correct=F)




              