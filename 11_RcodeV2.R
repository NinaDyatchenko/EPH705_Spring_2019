# Lecture 11 April 12th 2019
############## Arthritis data ------------------------------

data("Arthritis", package = "vcd")
head(Arthritis$Improved, 8)

# reordered levels, specify levels
Arthritis$Improved  <- ordered(Arthritis$Improved, 
                               levels = c("Marked", "Some", "None"))
# now ordered levels
head(Arthritis$Improved, 8)


library(vcd)
# gives table same as in SAS
structable (Improved ~ Sex + Treatment,  
            Arthritis)

# ifelse assings variable. If Female is true, assign 1, if not, assign 0 
# to change the female to 1 and male to 0 to match SAS output
Arthritis$Sex2 <- ifelse(Arthritis$Sex=="Female", 1, 0)


### proportional odds model
# need to specify Family
library(VGAM)
arth.po <- vglm(Improved ~ Sex2 + Treatment , 
                data = Arthritis,
                family = cumulative(parallel = TRUE))

summary(arth.po)
# now output matches SAS output from the lacture 11.

coef(arth.po, matrix = TRUE)

# not assuming proportional odds
# generalized logit model: parallel = False
arth.npo <- vglm(Improved ~ Sex + Treatment , 
                 data = Arthritis,
                 family = cumulative(parallel = FALSE))
arth.npo

coef(arth.npo, matrix = TRUE)

summary(arth.npo)

# testing proportional odds assumption
#  :: means this f-n fount in this package, not loaging whole package VGAM
VGAM::lrtest(arth.npo, arth.po)

### partial proportional odds model 
# parallel assumption on only treatment but not sex, sex is parallel=false
arth.ppo <- vglm(Improved ~ Sex + Treatment, 
                 data = Arthritis,
                 family = cumulative(parallel = FALSE ~ Sex))
coef(arth.ppo, matrix = TRUE)

summary(arth.ppo)



###### estimated proportions

covariates <- expand.grid(Treatment = c("Treated", "Placebo"),
                          Sex2 = c("Male", "Female")) 
# note sure if female and male are in the right place
                          
probs <- predict(arth.po, 
                 newdata = covariates, 
                 type = "response")

probs.covariates <- cbind (covariates, probs)




library(reshape2)
plotdat <- melt(probs.covariates,
                id.vars = c("Sex2", "Treatment"),
                measure.vars = c("None", "Some", "Marked"),
                variable.name = "Level",
                value.name = "Probability")

plotdat$group <- factor(paste0(plotdat$Sex,"_", plotdat$Treatment))

plotdat$group <- ordered (plotdat$group, 
                          levels = c("Female_Treated", "Male_Treated", 
                                     "Female_Placebo", "Male_Placebo") )


library(ggplot2)
ggplot(data=plotdat, aes(x = group , y= Probability, fill=Level)) +
  geom_bar(stat="identity", width = 0.5) + theme_bw()
# I get different result (plot) becasue she re-coded it during class
# See SAS p. 275 example
