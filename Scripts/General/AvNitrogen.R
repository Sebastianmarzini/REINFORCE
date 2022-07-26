rm(list=ls())

library(tidyverse)
library(dplyr)

## script for the calculation of available nitrogen (Nav) in relation to the forest types, kAvN max, kAvN min and scale

#ntf <- read.csv("Dati aree studio/Venosta/Nav_tipiforestali.csv") %>% rename(meanN = Nitrogen.mean..rounded.)

kAvN_max <- 100
kAvN_min <- 35
scale <- 5

for(i in 1:nrow(ntf)){
  ntf$Nav[i] <- ((kAvN_max - kAvN_min)/scale) * ntf$meanN[i] + kAvN_min
}

## N tolerance classes taken by the iLand documentation where:
## tolerant = 1
## somewhat_tolerant = 1.5
## intermediate = 2
## intermediate_intolerant = 2.2
## somewhat_intolerant = 2.5
## intolerant = 3

fn_data <- data.frame(respNitrogenClass = c("aN", "bN"), 
                      tolerant = c(-0.045, 10),
                      somewhat_tolerant = c(-0.05, 17.5),
                      intermediate = c(-0.055, 25),
                      intermediate_intolerant = c(-0.0575, 28.75),                    
                      somewhat_intolerant = c(-0.06, 32.5),
                      intolerant = c(-0.065, 40))

## calcolo fn per ogni classe e per ogni tipo forestale

for(i in 1:nrow(ntf)){
  ntf$fn_1[i] <- max((1-exp(fn_data$tolerant[1]*(ntf$Nav[i]-fn_data$tolerant[2]))), 0)
  ntf$fn_1.5[i] <- max((1-exp(fn_data$somewhat_tolerant[1]*(ntf$Nav[i]-fn_data$somewhat_tolerant[2]))), 0)
  ntf$fn_2[i] <- max((1-exp(fn_data$intermediate[1]*(ntf$Nav[i]-fn_data$intermediate[2]))), 0)
  ntf$fn_2.2[i] <- max((1-exp(fn_data$intermediate_intolerant[1]*(ntf$Nav[i]-fn_data$intermediate_intolerant[2]))), 0)
  ntf$fn_2.5[i] <- max((1-exp(fn_data$somewhat_intolerant[1]*(ntf$Nav[i]-fn_data$somewhat_intolerant[2]))), 0)
  ntf$fn_3[i] <- max((1-exp(fn_data$intolerant[1]*(ntf$Nav[i]-fn_data$intolerant[2]))), 0)
}

ntf <- ntf %>% rename("1.0" = fn_1,
                      "1.5" = fn_1.5,
                      "2.0" = fn_2,
                      "2.2" = fn_2.2,
                      "2.5" = fn_2.5,
                      "3.0" = fn_3)

ntf <- ntf %>% select(c(Forest.Type, Nav))

write.csv(ntf, "Dati aree studio/Venosta/Nav_tipiForestali.csv", row.names = FALSE)




    
