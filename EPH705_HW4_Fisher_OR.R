# Homework 4. Fishers test

# Agresti 2.30
FishersExact <- matrix( c(21, 2, 15, 3),
                        nrow = 2, ncol = 2, 
                        byrow = TRUE)
fisher.test(FishersExact, alternative = "greater")  # for Ha: theta > 1

# Agresti 2.33. Simpson
library("vcd")
# a) and b)
Murder <- array(c (19, 11, 132, 52, 0, 6, 9, 97), 
                 dim = c(2, 2, 2))
MurderThree <- as.table(Murder)
MurderThree
ftable(MurderThree)

# names to each dimention
dimnames(Murder) <- list (raceDef = c("W", "B"), 
                          penalty = c("y", "n"),
                          raceVic = c("W", "B"))
Murder <- as.table(Murder)
Murder

structable(. ~raceDef, data = Murder)

# b) Calculate conditional Odds ratio:

loddsratio(Murder, log = FALSE)
confint(loddsratio(Murder, log = FALSE))

# c) Marginal Odds ratio:

library(samplesizeCMH)

prop.table(Murder)
marginal_Murder <- margin.table(Murder, c(2, 1))
marginal_Murder
chisq.test(marginal_Murder)
odds.ratio(marginal_Murder)

prop.table(marginal_Murder)
mantelhaen.test(marginal_Murder)
mantelhaen.test(Murder)
