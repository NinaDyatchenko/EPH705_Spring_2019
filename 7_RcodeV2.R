# March 8th 2019
######################## model parametrization ----------------------------------
# expand.grid vives all combo of treatment and diagnosis table

#################  reference coding
temp <- expand.grid( treatment = c("A", "B", "C"), 
                     diagnosis = c("complicated", "uncomplicated") )

temp2 <- cbind (temp, cured = c(78, 101, 68, 40, 54, 34), 
                not.cured = c(28, 11, 46, 5, 5, 6))

temp2  # merged

temp2$total <- temp2$cured + temp2$not.cured

#  event/trial format. Total ppl for each group
uti.logist <- glm (cured / total ~ diagnosis + treatment , 
                   weight = total, 
                   data = temp2, 
                   family = binomial)

# getting param estimates
summary(uti.logist)

# gives the design matrix. Ref coding, indicators 1 or 0. Ref gropu is treat A
model.matrix (uti.logist)

################### our oun reference coding 

# efelse - first slot is condition. If first condition is met, 
# then the var is 1, if not met then condition is 0
temp2$trtA <- ifelse (temp2$treatment == "A", 1, 0)
temp2$trtB <- ifelse (temp2$treatment == "B", 1, 0)

temp2$trtA
temp2$trtB

temp2$diag <- ifelse (temp2$diagnosis == "complicated", 1, 0)

uti.logist2 <- glm (cured / total ~ diag + trtA + trtB + diag*trtA + diag*trtB,
                    weight = total, data = temp2, family = binomial)

summary(uti.logist2)
# same as ref coding in SAS 

model.matrix (uti.logist2)

################ effect coding ( +/- )

temp3 <- temp2[, c("treatment", "diagnosis", "cured", "not.cured", "total")]

temp3$diag <- ifelse (temp3$diagnosis == "complicated", 1, -1)

#newvar [oldvar == value1] = newvalue1
# coding contrast between groups
# old var equals A then assign 1 to the new var
# same as effect coding is SAS
temp3$trtA [temp3$treatment == "A"] = 1
temp3$trtA [temp3$treatment == "B"] = 0
temp3$trtA [temp3$treatment == "C"] = -1

temp3$trtB [temp3$treatment == "A"] = 0
temp3$trtB [temp3$treatment == "B"] = 1
temp3$trtB [temp3$treatment == "C"] = -1

uti.logist3 <- glm (cured / total ~ diag + trtA + trtB + diag*trtA + diag*trtB,
                    weight = total, data = temp3, family = binomial)

summary(uti.logist3)

model.matrix (uti.logist3)

library(gmodels)

# first vs. second group. specify contrast that we want. 
#  This on is first row - secind row
test <- estimable (uti.logist3, c (0, 0, 1, -1, 1, -1 ), conf.int = 0.95)

#  exponentiate
oddsratio <- exp(test.grp5)

oddsratio

################################ cell mean coding 

temp4 <- temp2[, c("treatment", "diagnosis", "cured", "not.cured", "total")]

#  6 groups
group <- diag(6)
colnames (group) <- c(1:6)

uti.logist4 <- glm (cured / total ~ group +0,
                    weight = total, data = temp2, family = binomial)

# estimates are the means for the groups
summary(uti.logist4)

# odds ratio
library(gmodels)
test <- estimable (uti.logist4, c (1, -1, 0, 0, 0, 0 ), conf.int = 0.95)

oddsratio <- exp(test)

oddsratio


############################ goodness of fit for continuous data
####### CA with Age data 

ca <- read.csv ("ca_age.csv")

ca.logist <- glm (ca ~ sex + ecg + age, data = ca, family = binomial)

summary(ca.logist)

# Hosmer & Lemeshow statistic
library(ResourceSelection)
hoslem.test (ca.logist$y, fitted(ca.logist), g = 10)




###################################### ROC analysis 

dat <- read.csv ("7_data_roc.csv")
dat$no.disease <- dat$n - dat$disease
dat

# convert to case format
library(vcdExtra)

# expand data frame
cases <- expand.dft (dat, freq = "disease")
cases
cases$disease <- 1
cases <- cases[, c("age", "disease")]


controls <- expand.dft (dat, freq = "no.disease")
controls$disease <- 0
controls <- controls[, c("age", "disease")]
controls
all <- rbind (cases, controls)

library(pROC)
roc1 <- roc( response = all$disease, predictor = all$age , plot = TRUE)

roc1$sensitivities

1-roc1$specificities


