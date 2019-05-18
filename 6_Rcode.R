setwd("C:/Users/lxw391/Box Sync/TEACHING/2018-LW-lectures")
# 2/22/2019

###################### CA data example - grouped binomial format

temp <- expand.grid( ECG = c("<0.1 ST segment depression", ">= 0.1 ST segment depression"),
                     sex = c("female", "male"))

temp2 <- cbind (temp, disease = c(4,8,9,21), total = c(15, 18,18,27))

ca.logist <- glm (disease / total ~ sex + ECG, weight = total, 
                  data = temp2, family = binomial)

summary(ca.logist)

model.matrix (ca.logist)

############# frequency format 
temp2$no.disease <- temp2$total - temp2$disease

library(tidyr)
temp3 <- gather (temp2, key=status, value = count, disease, no.disease)

ca.logist2 <- glm (as.factor(status) ~ sex + ECG, weights = count, 
                   data = temp3, family = binomial)
summary(ca.logist2)

model.matrix (ca.logist2)

?binomial

# reorder the levels of response variables
# --- which response level is R modelling? 
# As a factor: 'success' is interpreted as the factor not having the first level 
# (and hence usually of having the second level).

# order factors
temp3$status2 <- as.factor(temp3$status)

levels (temp3$status2)  

temp3$status2 <- ordered(temp3$status2, levels = c("no.disease", "disease"))

levels(temp3$status2)

ca.logist <- glm (as.factor(status2) ~ sex + ECG, weights = count, data = temp3, family = binomial)
summary(ca.logist)

# another way to model prob of disease
status3 <- ifelse (temp3$status == "disease", 1, 0)

ca.logist <- glm (status3 ~ sex + ECG, weights = count, data = temp3, family = binomial)
summary(ca.logist)


#### slide 5: wald test
ca.logist <- glm (status3 ~ sex + ECG, weights = count, data = temp3, 
                  family = binomial)

library(aod)
wald.test (b = coef(ca.logist), Sigma = vcov(ca.logist), Terms = 2:3 )

# manually
Lmatrix <- matrix ( c(0, 1, 0, 0, 0, 1), nrow=2, ncol = 3, byrow = TRUE)

Lmatrix

wald.test (b = coef(ca.logist), Sigma = vcov(ca.logist), L = Lmatrix )

### slide 7: likelihood ratio test

intercept <- rep(1, nrow(temp3))

mod1 <- glm (status3 ~ offset(intercept), weights = count, 
             data = temp3, family = binomial)

mod2 <- glm (status3 ~ sex + ECG, weights = count, 
             data = temp3, family = binomial)

anova (mod1, mod2, test = "LRT" )  # likelihood ratio test. Compating -2logs)


### slide 6: score test for the two models. Model 2 fits better than model 1 (p val is small)tests if sex and ecg are the same
anova (mod1, mod2, test = "Rao" )


############# slide 11: confidence interval for effect estimates. 
mod2 <- glm (status3 ~ sex + ECG, weights = count, 
             data = temp3, family = binomial)


#  profile-likelihood CI
confint (mod2)

cbind(coef(mod2), confint(mod2))  

# wald CI
confint.default(mod2)  

cbind(coef(mod2), confint.default(mod2))

# wald CI manually
table <- data.frame(summary(mod2)$coefficients)

table$lower.ci <- table$Estimate - 1.96 * table$Std..Error
table$upper.ci <- table$Estimate + 1.96 * table$Std..Error

table

############ slide 13: confidence interval for odds ratios 
exp(confint.default(mod2))

########### slide 14: confidence interval for predicted probabilities
groups <- temp3[ , c("ECG", "sex", "status")]  # specify groups

pred <- predict (mod2, groups, type = "link", se.fit = TRUE)

pred

logit <- cbind (groups, pred$fit, pred$fit - 1.96*pred$se.fit, pred$fit + 1.96*pred$se.fit)

colnames (logit) [5:6] <- c("lower.ci", "upper.ci") # shows logit, not probabilities

logit

probs <- exp(logit[, 4:6])/ (1+exp(logit[, 4:6]))  # converting from logit to prob.

probs.all <- cbind (groups, probs)

probs.all <- probs.all[order(probs.all$sex, probs.all$ECG, probs.all$status) ,]

probs.all






