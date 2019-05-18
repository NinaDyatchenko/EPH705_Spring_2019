### EPH705 Homework 10, April 12th 2019

coffee <-  matrix(c(93, 9, 17, 6, 10, 17, 46, 11, 4, 4, 44, 11, 155, 9, 
                    12, 7, 0, 9, 15, 2, 10, 9, 12, 2, 27),
                  nrow =5 , 
  dimnames = list ("first" = c("High", "Tester's", "Sanka", "Nescafe", "Brim"), 
                   "Second" = c("High", "Tester's", "Sanka", "Nescafe", "Brim")))
coffee
  
library(vcd)
Kappa(coffee)

