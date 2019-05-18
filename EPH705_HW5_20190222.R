# EPH705 HW 5
# Feb 22, 2019

creditcards <- data.frame(inc= c(24, 27, 28, 29, 30, 31, 32, 33, 34, 35, 38, 
                                 39, 40, 41, 42, 45, 48, 49, 50, 52, 59, 60, 
                                 65, 68, 70, 79, 80, 84, 94, 120, 130),
                             
                             ncard= c(0, 0, 2, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 
                                      0, 1, 0, 0, 2, 0, 0, 2, 6, 3, 3, 0, 0, 0, 
                                      0, 6, 1),
                             
                             ncase= c(1, 1, 5, 3, 9, 5, 8, 1, 7, 1, 3, 2, 5, 2, 
                                      2, 1, 1, 1, 10, 1, 1, 5, 6, 3, 5, 1, 1, 1,
                                      1, 6, 1))
creditcards
creditcards_logit <- glm (cbind(ncard, ncase-ncard) ~ inc, 
                 data = creditcards, family = binomial)

summary(creditcards_logit)

cbind(creditcards$ncard, creditcards$ncase- creditcards$ncard)
