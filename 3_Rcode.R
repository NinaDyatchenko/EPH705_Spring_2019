# lecture 3 2/1/2019 Test for indipendance

setwd("C:/Users/lxw391/Box Sync/TEACHING/2019-LW-lectures")

# frequency form
respire_test <- data.frame(treat = c("placebo", "test", "test2"), # column 1 = rows: vectors with 2 levels
               outcome = c("f", "u"), # vector with 2 levels
            count = c(16, 40, 48, 20))
# expand.greed 
respire_test


respire <- data.frame(
    expand.grid (treat = c("placebo", "test"), # column 1 = rows: vectors with 2 levels
                 outcome = c("f", "u"))  , # vector with 2 levels
                 count = c(16, 40, 48, 20))
# expand.greed 
respire          
names (respire)    
str(respire)    

########## hypertension example 
# 1. construct table
hypertension <- matrix ( c (50, 150, 140, 100), 2, 2)
dimnames (hypertension) <- list (exercise = c("yes", "no"), 
                                 hypert = c("yes", "no")
                                 )
hypertension <- as.table (hypertension)

hypertension


# 2. look at proportions. Takes out count and rows
library(gmodels)
CrossTable(hypertension, prop.chisq = TRUE, 
           prop.c= FALSE,
           prop.r = FALSE,
           format = "SPSS")

#3. chi-sq test of independence 
summary(hypertension)

library(vcd)
assocstats(hypertension)




# table form
respire <- matrix ( c(16, 40, 48, 20), 2,2)

dimnames (respire) <- list ( treat = c("placebo", "test"), 
                             outcome = c("f", "u"))   
  
respire <- as.table(respire)
  
# proportions
respire <- margin.table(respire, 1:2)
library(gmodels)
CrossTable(respire, prop.chisq = FALSE, format = "SPSS")
CrossTable(respire, 
           prop.chisq = FALSE, 
           expected = TRUE,
           prop.c= FALSE,
           prop.r = FALSE,
           format = "SPSS")
CrossTable(respire, prop.chisq = TRUE, format = "SPSS")

summary(respire)


####### ordinal outcome
arth <- matrix (c (13, 29, 7, 7, 21, 7), 2, 3)

dimnames (arth) <- list (treat = c("active", "placebo"), 
                         outcome = c("none", "some", "marked")
                         )
CrossTable(arth, prop.chisq = FALSE, prop.c = FALSE)

library(vcd)

assocstats(arth)

library(vcdExtra)

# use midrank scores for columns, equivalent to SAS option scores = rank
CMHtest(arth, cscores = "midrank")

# use table scores for columns (default in sas and R)
CMHtest (arth)

############## chpater 2 of R book
categories <- expand.grid(sex=c("female", "male"),
            party=c("dem", "indep", "rep"))

count <- c(279,165,73,47,225,191)

df <- cbind (categories, count)

df2 <- data.frame (categories, count)

# chande row and columns levels
levels(respire$treat) <- c("test", "placebo")
respire
