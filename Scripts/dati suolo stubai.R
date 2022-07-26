rm(list=ls())

library(raster)
library(rgdal)
library(sp)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(colorspace)
library(RSQLite)
library(tidyverse)
library(stringr)
library(compare)

# # img con coordinate e colonna con collegamento a file xlsx contenente i dati per tessitura, carbonio e azoto
# soil_param <- raster("Dati aree studio/Stubai/Georg/Soil_data_STUBAI/Soil_data_Stubai.img")
# stc <- as.data.frame(soil_param, xy=TRUE) %>% rename (Trajectory = Soil_data_STUBAI)
# # file xlsx con dati vari suolo
# soildata <- as.data.frame(readxl::read_xlsx("Dati aree studio/Stubai/Georg/C_N_Texture_FIN.xlsx"))
# sdf <- left_join(stc, soildata, by = "Trajectory")
# 
# # tif con dati profondità suolo
# soil_depth <- raster("Dati aree studio/Stubai/Georg/Soil_data_STUBAI/Soil_depth.tif") 
# sd <- as.data.frame(rasterToPoints(soil_depth))
# 
# sdf$xy <- paste(sdf$x, sdf$y, sep="")
# sd$xy <- paste(sd$x, sd$y, sep="")
# 
# compare(sdf$xy, sd$xy, ignoreOrder = TRUE)
# 
# df <- left_join(sdf, sd, by = c("xy"))
# 
# df <- df %>% select(-c(xy, y.y, x.y)) %>% rename(x = x.x, y = y.x)

# img con coordinate e colonna con collegamento a file xlsx contenente i dati per tessitura, carbonio e azoto
soilCtext <- aggregate(raster("Dati aree studio/Stubai/Georg/Soil_data_STUBAI/scarbText_buffer.tif"), 10)
soilCtext <- as.data.frame(soilCtext, xy=TRUE) %>% rename (Trajectory = scarbText_buffer)

# file xlsx con dati vari suolo: lo carico e faccio join con raster
soildata <- as.data.frame(readxl::read_xlsx("Dati aree studio/Stubai/Georg/C_N_Texture_FIN.xlsx"))
soilCtextDF <- left_join(soilCtext, soildata, by = "Trajectory")
soilCtextDF <- soilCtextDF[!is.na(soilCtextDF$Trajectory),]

plot(sDepth)

# tif con dati profondità suolo
soil_depth <- aggregate(raster("Dati aree studio/Stubai/Georg/Soil_data_STUBAI/sdepth_buffer.tif"), 10)
sDepth <- as.data.frame(rasterToPoints(soil_depth))

soilCtextDF$x <- round(soilCtextDF$x, 4)
soilCtextDF$y <- round(soilCtextDF$y, 4)
sDepth$x <- round(sDepth$x, 4)
sDepth$y <- round(sDepth$y, 4)

finalDF <- left_join(soilCtextDF, sDepth, by = c("x", "y"))
finalDF <- finalDF %>% rename(Depth = sdepth_buffer)

write.csv(finalDF, "Dati aree studio/Stubai/Georg/Soil_data_STUBAI/soil_data_Stubai.csv", row.names = FALSE)
