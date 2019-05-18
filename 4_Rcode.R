# 2/8/2019
# lecture 4

################## fisher's exact test - dataset from Stokes sec 2.3 

# 1. construct table
severe <- matrix ( c(10, 2, 2, 4), 2, 2)

dimnames (severe) <- list (treat = c("Test", "Control"), 
                           outcome = c("f", "u")
                           ) 

severe <- as.table (severe)

severe

# 2. look at proportions
library(gmodels)
CrossTable(severe, prop.chisq = FALSE, prop.c = FALSE, prop.t = FALSE,  format = "SAS")

# 3. fisher's exact test
fisher.test (severe)
# one of the row added twice, that is why the the prob doesn't sum up to 1 
fisher.test (severe, alternative = "greater")  # Right-sided Pr>= F in SAS
fisher.test (severe, alternative = "less")     # Left-sided Pr <= F

#################### stratified table - dataset from Stokes 3.2.1 

# 1. respiretory outcome example. Vn11
# array because it a set of 2x2 tables. So 3 dimentions. @ tables, in each 2 row and 2 columns

respire <- array (c (29, 14, 16, 31, 
                     37, 24, 8, 21), dim = c(2, 2, 2))
#names to each dimention
dimnames (respire) <- list (treatment = c("test", "placebo"), 
                            response = c("y", "n"), 
                            center = c("1", "2")
                            )

respire <- as.table (respire)
respire
library(vcd)
assocstats(respire)  # statistics analysis of each table

library(vcdExtra)
CMHtest (respire, overall = TRUE) # gives both central spectific and 

library(DescTools)
BreslowDayTest(respire)


# odds ratio. Also gives CMH test
mantelhaen.test(respire, alternative = "two.sided",
                correct = FALSE, exact = FALSE, conf.level = 0.95)

Ora <- loddsratio(respire, log = FALSE) 

plot (Ora, bars = FALSE, whiskers = 0.05)

