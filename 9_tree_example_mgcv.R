# March 29th 2019

library(mgcv)
ct1 <- gam(Volume ~ s(Height) + s(Girth), 
           family=Gamma(link=log),
           data=trees) 
ct1  # default generalized approch

par(mfrow=c(1,2))
plot(ct1,residuals=TRUE,
     pch=19) ## calls plot.gam

gam.check(ct1)

ct1 <- gam(Volume~s(Height)+s(Girth),
           family=Gamma(link=log),
           data=trees,
           method="ML")
ct1
summary(ct1)

## create dataframe of new values. New DATA
pd <- data.frame(Height=c(75,80),Girth=c(12,13))
pd
# ct1 is model object
predict(ct1,newdata=pd)

predict(ct1,newdata=pd,se=TRUE)

predict(ct1,newdata=pd,se=TRUE,type="terms")

par(mfrow=c(1,2))

plot(ct1,
     shade=TRUE,
     seWithMean=TRUE,
     scale=0)

plot(ct1,
     seWithMean=TRUE,
     scale=0)
