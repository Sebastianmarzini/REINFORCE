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

x <- raster("Dati aree studio/Stubai/STUBAI/Georg/Soil_data_STUBAI/Soil_data_Stubai.img")

plot(x)

df <- as.data.frame(x, xy=TRUE) %>% rename (Trajectory = Soil_data_STUBAI)

soildata <- as.data.frame(readxl::read_xlsx("Dati aree studio/Stubai/STUBAI/Georg/C_N_Texture_FIN.xlsx"))

df <- left_join(df, soildata, by = "Trajectory")

soil_depth <- raster("Dati aree studio/Stubai/STUBAI/Georg/Soil_data_STUBAI/Soil_depth.tif") ## problema crs

sd <- rasterToPoints(soil_depth)
sd_df <- as.data.frame(sd)

df$xy <- paste(df$x, df$y, sep="")
sd_df$xy <- paste(sd_df$x, sd_df$y, sep="")

df <- left_join(df, sd_df, by = "xy")

df <- df %>% select(-c(xy, y.y, x.y)) %>% rename(x = x.x, y = y.x)
