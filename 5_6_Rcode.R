setwd("C:/Users/lxw391/Box Sync/TEACHING/2018-LW-lectures")

################# R book: section 7.2.1 visulize logistic regression
library(vcd)
data("Arthritis", package = "vcd")
Arthritis$Better <- as.numeric(Arthritis$Improved > "None")

library(popbio)
logi.hist.plot(independ = Arthritis$Age, 
               depend = Arthritis$Better, 
               type = "hist",
              counts = TRUE, ylabel = "Probability (Better)",
                    xlab = "Age",
                    col.hist = "lightblue")
# as age increases, the likelihood to get better increases
Arthritis
# coefficients for logistic regression

arth.logistic <- glm(Better ~ Age, data = Arthritis, family = binomial)
arth.logistic
summary(arth.logistic)

model.matrix (arth.logistic)  # everybody have age


library(lmtest)
coeftest(arth.logistic)

summary(arth.logistic)

coef(arth.logistic)  # extract only coefficient

# odds of a better response for 1 year increase in age
exp(coef(arth.logistic))

# odds of a better response for 10 year increase in age
exp(10 * coef(arth.logistic)["Age"])  # 10 years olde is 1.6 more likely to get better




################### R book: sec 7.2.4 grouped binomial data

data("SpaceShuttle", package = "vcd")
SpaceShuttle
shuttle.mod <- glm(cbind(nFailures, 6 - nFailures) ~ Temperature,
                   data = SpaceShuttle, na.action = na.exclude,
                   family = binomial)
shuttle.mod
SpaceShuttle$trials <- 6
shuttle.modw <- glm(nFailures / trials ~ Temperature, weight = trials,
                    data = SpaceShuttle, na.action = na.exclude,
                    family = binomial)

# modeling failure as f-n of temp.
summary(shuttle.modw)  # temperature has negative effect


###################### CA data example
temp <- expand.grid( ECG = c("<0.1 ST segment depression", ">= 0.1 ST segment depression"),
                     sex = c("female", "male"))

temp2 <- cbind (temp, disease = c(4,8,9,21), total = c(15, 18,18,27))

ca.logist <- glm (disease / total ~ sex + ECG, weight = total, 
                  data = temp2, family = binomial)

summary(ca.logist)  # to look at the model
temp2
#############
temp2$no.disease <- temp2$total - temp2$disease

library(tidyr)
temp3 <- gather (temp2, key=status, value = count, disease, no.disease)

temp3
ca.logist2 <- glm (as.factor(status) ~ sex + ECG, weights = count, data = temp3, family = binomial)
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

# reorder factors
temp3$status2 <- ordered(temp3$status2, levels = c("no.disease", "disease"))

levels(temp3$status2) # no disease goes first

ca.logist <- glm (as.factor(status2) ~ sex + ECG, weights = count, data = temp3, family = binomial)
summary(ca.logist) # now the signes of estimates are same as in sas
temp3$status2

# another way to model prob of disease. Ifelse changes status to indicator variable
status3 <- ifelse (temp3$status == "disease", 1, 0)
status3
ca.logist <- glm (status3 ~ sex + ECG, weights = count, data = temp3, family = binomial)
summary(ca.logist)

str(Arthritis)

