# Homework 3
# Agresti 2.17. Find P-value using Chi-square

HAdata <- matrix( c(189, 10845, 104, 10933),
                 nrow = 2, ncol = 2, byrow = TRUE)

dimnames(HAdata) <- list (Treatment = c("Aspirin", "Placebo"),
                           outcome = c("Event", "No Event"))
chisq.test(HAdata, correct = FALSE)


# Agresti 2.22 Conduct a test for independance

PSYdata <- matrix( c(105, 8, 12, 2, 18, 19, 47, 52, 0, 13),
                   nrow = 5, ncol = 2, byrow = TRUE)

dimnames(PSYdata) <- list (Treatment = c("Aspirin", "Placebo"),
                          outcome = c("Shizo", "Affective dis.", "Neurosis", 
                                      "Person. dis.", "Special sympt."))
chisq.test(PSYdata, correct = F)

# Agresti 2.23
BeliefDdata <- matrix( c(178, 138, 108, 570, 648, 442, 138, 252, 252),
                   nrow = 3, ncol = 3, byrow = TRUE)

dimnames(BeliefDdata) <- list (Treatment = c("Less", "Junior", "Grad"),
                           outcome = c("Fundament", "Moderate", "Liberal"))
library(gmodels)
CrossTable(BeliefDdata, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           asresid = TRUE, format = "SPSS", prop.t = FALSE)
library(vcd)
assocstats(BeliefDdata)
