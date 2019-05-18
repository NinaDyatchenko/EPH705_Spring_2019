setwd("C:/Users/lxw391/Box Sync/TEACHING/2018-LW-lectures")

library(gee)

depress<- read.csv ("12_depress.csv")
m <- gee(formula = outcome ~ diagnose + treat + time, 
         id=case, data=depress, 
         family=binomial, 
         corstr="exchangeable")
summary(m)
# doesn't dive p-value, give SE


# estimate
est <- data.frame(m$coefficients)
est

# pull out cars.
se <- data.frame(sqrt(diag(m$robust.variance)))
se

## z scores
wald.z <- est / se
wald.z

## now compute p-vals
p.value <- 2*( 1- pnorm(abs(wald.z[,1])))

names(p.value) <- row.names(wald.z)
p.value

# off-diagonal are the covariets of betas
m$robust.variance


