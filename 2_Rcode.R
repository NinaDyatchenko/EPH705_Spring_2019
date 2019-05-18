
library(vcdExtra)

########### chapter 4 of R book

# logodds is symmetric
p <- c(0.05, .1, .25, .50, .75, .9, .95)
odds <- p / (1-p)
logodds <- log(odds)
dat <- data.frame(p, odds, logodds)

#### reproduce sas output

# stress example

stress_mat <- matrix(c (150, 100, 50, 140),
                     nrow = 2, ncol = 2, byrow = TRUE)

dimnames (stress_mat) <- list (stress = c("low", "high"),
                               outcome = c("f", "u"))


stress_mat

stress_tab <- as.table(stress_mat)
str(stress_tab)

# calculation by hand
OR <- (48*94)/(12*96)

OR

LOR <- log((48*94)/(12*96))

LOR

# using functions
# approach 1
OR <- oddsratio(stress_tab, log = FALSE)
OR
ciOR <- confint(OR)
ciOR


# approach 2
LOR <- oddsratio(stress_tab)

ciLOR <- confint(LOR)  #95% conf interval for LOR
ciLOR

ciOR <- exp(ciLOR)
ciOR


