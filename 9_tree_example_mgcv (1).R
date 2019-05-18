library(mgcv)
ct1 <- gam(Volume ~ s(Height) + s(Girth), family=Gamma(link=log),data=trees) 

ct1

par(mfrow=c(1,2))
plot(ct1,residuals=TRUE,pch=19) ## calls plot.gam

gam.check(ct1)

ct1 <- gam(Volume~s(Height)+s(Girth),
           family=Gamma(link=log),
           data=trees,
           method="ML")
ct1
summary(ct1)

## create dataframe of new values...
pd <- data.frame(Height=c(75,80),Girth=c(12,13))
predict(ct1,newdata=pd)
predict(ct1,newdata=pd,se=TRUE)

predict(ct1,newdata=pd,se=TRUE,type="terms")

par(mfrow=c(1,2))
plot(ct1,shade=TRUE,seWithMean=TRUE,scale=0)
pd


######## Homework for Lecture 9 ####### April 4th 2019
log.volume <- log(trees$Volume)
log.volume

# Smooth
model_vol_grth <- gam(log.volume ~ s(Height) + s(Girth), 
                      data = trees,
                      family=gaussian(link=log))
model_vol_grth
# Smooth penalized cubic regression spline

model_penal <- gam(log.volume ~ s(Height) + s(Girth), 
                    data = trees,
                    family=gaussian(link=log),
                   method = "REML", 
                   bs = "cr")
model_penal

## Plot fitted functions
plot(model_vol_grth,
     residuals=TRUE,
     pch=19) ## calls plot.gam

plot(model_penal,
     residuals=TRUE,
     pch=19)

# Predict volume given hight and girth for REML model
vol_predict <- data.frame(Height = 70,
                      Girth= 10)
predict(model_penal,
        newdata = vol_predict)

