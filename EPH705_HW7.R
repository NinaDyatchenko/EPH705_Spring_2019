# EPH705
# Homework 7 MArch 28 2019


####### Agresti 5.25 #######33
Lymph <- c(1, 1, 1, 1, -1, -1, -1, -1)
Sex <- c(1, 1, -1, -1, 1, 1, -1, -1)
Pathology <-  c(1, -1, 1, -1, 1, -1, 1, -1)
response <- c(3, 2, 4, 1, 5, 3, 5, 6)
count <- c(0, 0, 0, 0, 0, 2, 4, 11)


LogXact <- data.frame(Lymph, Sex, Pathology, response, count)
LogXact

# Firth method
library(logistf)
logistf (response ~ gender + region, weights = count, 
         family = binomial, 
         data = LogXact_1)


lr_1 <- glm (response ~ Lymph, 
           weights = count, 
           family = binomial, data = LogXact)
summary(lr_1)

Lymph <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
Sex <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1)
Pathology <-  c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 )
count <- c(3, 0, 2, 0, 4, 0, 1, 0, 5, 0, 3, 0, 5, 0, 6, 0)
response <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0)


LogXact_1 <- data.frame(Lymph, Sex, Pathology, response, count)
LogXact_1
total_resp <- sum(response)
total_resp

# Firth method
library(logistf)
logistf (response ~ Lymph + Sex + Pathology, 
         weights = count, 
         family = binomial, 
         data = LogXact_1)

with(LogXact_1, table())
lr_1_1 <- glm (response ~ Lymph, 
             weights = count, 
             family = poisson, data = LogXact_1)
summary(lr_1_1)

lr_1_2 <- glm (response ~ Lymph + Sex + Pathology, 
               weights = count, 
               family = poisson, data = LogXact_1)
summary(lr_1_2)


Lymph <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
Sex <- c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
Pathology <-  c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
count <- c(3, 0, 2, 0, 4, 0, 1, 0, 5, 0, 3, 0, 5, 0, 6, 0)
response <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,0, 1, 0, 1, 0)


LogXact_1 <- data.frame(Lymph, Sex, Pathology, response, count)
LogXact_1


#####   Agresti 8.1 ########
# McNemar test ###

diabetes_MI <- matrix(c(9, 37, 16, 82),
         nrow = 2,
         dimnames = list("MI Controls" = c("Diabetes", "No Diabetes"),
                         "MI Cases" = c("Diabetes", "No Diabetes")))
diabetes_MI

mcnemar.test(diabetes_MI, correct = FALSE)
