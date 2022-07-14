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

# img con coordinate e colonna con collegamento a file xlsx contenente i dati per tessitura, carbonio e azoto
soil_param <- raster("Dati aree studio/Stubai/Georg/Soil_data_STUBAI/Soil_data_Stubai.img")
stc <- as.data.frame(soil_param, xy=TRUE) %>% rename (Trajectory = Soil_data_STUBAI)
# file xlsx con dati vari suolo
soildata <- as.data.frame(readxl::read_xlsx("Dati aree studio/Stubai/Georg/C_N_Texture_FIN.xlsx"))
sdf <- left_join(stc, soildata, by = "Trajectory")

# tif con dati profondità suolo
soil_depth <- raster("Dati aree studio/Stubai/Georg/Soil_data_STUBAI/Soil_depth.tif") 
sd <- as.data.frame(rasterToPoints(soil_depth))

sdf$xy <- paste(df$x, df$y, sep="")
sd$xy <- paste(sd$x, sd$y, sep="")

df <- left_join(df, sd_df, by = "xy")

df <- df %>% select(-c(xy, y.y, x.y)) %>% rename(x = x.x, y = y.x)
