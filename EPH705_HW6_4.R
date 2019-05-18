# Agresti 4.1

canremi <- data.frame(
  LI = c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 32, 34, 38),
  ncase = c(2, 2, 3, 3, 3, 1, 3, 2, 1, 1, 1, 1, 1, 3),
  nremi = c(0, 0, 0, 0, 0, 1, 2, 1, 0, 1, 1, 0, 1, 2)
)
canremi

canremi_logit <- glm(cbind(nremi, ncase-nremi) ~ LI, 
                     data = canremi, family = binomial)
summary(canremi_logit)
canremi_logit_null <- glm(cbind(nremi, ncase-nremi) ~ 1, 
                     data = canremi, family = binomial)

summary(canremi_logit_null)
anova(canremi_logit_null, canremi_logit, test = "Chisq")

# Agresti 4.4

snoring <- data.frame(snor = c(0, 2, 4, 5, 0, 2, 4, 5),
                       hd = c("yes", "yes", "yes", "yes", "no", "no", "no", "no"),
                       count = c(24, 35, 21, 30, 1355, 603, 192, 224))

snoring_logit <-  glm(hd ~ snor, weight = count, 
                      family = binomial, data = snoring)
snoring_logit_null <-  glm(hd ~ 1, weight = count, 
                      family = binomial, data = snoring)
summary(snoring_logit_null)
summary(snoring_logit)
snoring_logit
anova(snoring_logit, snoring_logit_null, test = "Chisq")

?glm
