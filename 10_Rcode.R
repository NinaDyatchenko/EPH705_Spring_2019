
setwd("C:/Users/lxw391/Downloads")
#setwd("C:/Users/lxw391/Box Sync/TEACHING/2018-LW-lectures")

#################### 1. aligator example
alligator <- read.csv ("10_data_aligator.csv")

class(alligator$choice)

levels(alligator$choice)  # 3 levels ( F, I and O)

# re-level O as reference
alligator$choice <- relevel(alligator$choice, ref = "O")  

library(nnet)

genlogit <- multinom(choice ~ length, 
                     data = alligator, 
                     Hess = TRUE)

summary(genlogit, Wald.ratios = TRUE)

stats <- summary(genlogit, Wald = TRUE)
stats

wald.z <- stats$Wald.ratios
wald.z

# Given the wald.z = z-score
# Compute p-value
p.value <- 2*( 1- pnorm(abs(wald.z)))
p.value



######## odds ratio estimate
est <- data.frame(stats$coefficients)
est
se <- data.frame(stats$standard.errors)

# bind the columns:
est.se <- data.frame (est$length, se$length)
est.se

# give column names:
colnames(est.se) <- c("estimate", "se")

est.se$ci.lower <- est.se$estimate - 1.96*est.se$se
est.se$ci.upper <- est.se$estimate + 1.96*est.se$se


# for OR, exponentiate the whole thing:
odds.ratio <- exp(est.se)
odds.ratio

row.names (odds.ratio) <- row.names(wald.z)
odds.ratio  # similar output as in SAS



############ plot estimated probabilities 

#fit <- data.frame(aligator$length, predict(genlogit, length, type = "probs"))

predictor <- data.frame(alligator$length)
colnames(predictor) <- "length"

# to presict probability for all alligators
probs <- predict(genlogit, 
                 newdata = predictor, 
                 type = "probs")

length.probs <- data.frame (predictor, probs)
colnames (length.probs) <- c("length", "other", "fish", "invertebrate")
length.probs # gives prediscted probabilities for Other Fish adn Invert.


# from wide to tall dataset
library(reshape2)

# melt to transform
fit2 <- melt(length.probs,
             measure.vars = c("fish", "other", "invertebrate"),
             variable.name = "food.choice",
             value.name = "probability")
#levels(fit2$food.choice) <- c("fish", "other", "invertebrate")
fit2

library(ggplot2)

# to export plot in pdf format. 
# Has to match with dev.off()
pdf ("figure.pdf")  

# the plot:
gg <- ggplot(fit2,
             aes(x = length, y = probability, colour = food.choice)) + 
  geom_line(size = 1) + theme_bw() +
  scale_x_continuous(limits = c(1, 4)) +
  scale_y_continuous(limits = c(0, 1))   

print (gg)
gg
dev.off()  # device off to match pdf
str(fit2)

