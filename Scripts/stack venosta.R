rm(list=ls())

library(sp)
library(raster)
library(rgdal)
library(plyr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(colorspace)
library(RSQLite)

# 1 Creation of the stack with the different rasters ----

#wd <- setwd("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/VAL_VENOSTA/GIS")

ilandru <- raster("iland.ru.id.tif")
dem <- raster("100m_DEM_20222403_1549.tif")
forest <- projectRaster(raster("tipiforestali_10m.tif"), dem, method = "ngb")
slope <- raster("100m_SLOPE_20222403_1549.tif")
aspect <- raster("100m_ASPECT_20222403_1549.tif")
depth <- raster("depth_100m.tif")
sand <- raster("sand_100m.tif")
silt <- raster("silt_100m.tif")
clay <- raster("clay_100m.tif")
bodenlands <- raster("bodenland_100m.tif")

dem <- projectRaster(dem, ilandru)
forest <- projectRaster(forest, ilandru)
slope <- projectRaster(slope, ilandru) 
aspect <- projectRaster(aspect, ilandru)
depth <- projectRaster(depth, ilandru)
sand <- projectRaster(sand, ilandru)
silt <- projectRaster(silt, ilandru)
clay <- projectRaster(clay, ilandru)
bodenlands <- projectRaster(bodenlands, ilandru)

st <- raster::stack(ilandru, forest, bodenlands, dem, slope, aspect, depth, sand, silt, clay)
plot(st)

writeRaster(st, "stackVenosta_100m.tif")

# 2 Data preparation of the stack---- 

dem_stack <- stack("stackVenosta_100m.tif") 
names(dem_stack) <- c("r_units", "elevation","waldtyp", "slope", "aspect", "depth", "sand", "silt", "clay", "bodenlands")
dem_stack
dem_stack_point <- rasterToPoints(dem_stack)
dem_stack_df <- as.data.frame(dem_stack_point)
dem_stack_df_red <- dem_stack_df[!is.na(dem_stack_df$elevation),]
dem_stack_df_red["waldtyp"][dem_stack_df_red["waldtyp"] == 128 | is.na(dem_stack_df_red["waldtyp"])] <- "nf"
dem_stack_df_red$depth = dem_stack_df_red$depth*1000
plot(rasterFromXYZ(dem_stack_df_red[,c(1,2,5)])) ##test plot

fortypes <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/VAL_VENOSTA/GIS/tipiforestali_venosta_raster10m.csv")

ST_raster <- merge(dem_stack_df_red, fortypes, by = 'waldtyp')
ST_raster <- rename(ST_raster, "tipi_for" = "cod")
ST_raster <- ST_raster%>% select(-waldtyp)

#creation of the dataset of the bodenlands with their codes
bl <- projectRaster(raster("VAL_VENOSTA/GIS/bodenland_100m.tif"), dem_stack)
b <- rasterToPoints(bl)
b <- as.data.frame(b)
b <- b %>% rename(bcode = bodenland_100m)
b$bcode <- round(b$bcode, digits = 0)

b1 <- read.csv("VAL_VENOSTA/GIS/bodenlands_codes.csv")
b1 <- b1 %>% rename(bcode = Boden_code)
bjoint <- left_join(b, b1)
plot(rasterFromXYZ(bjoint[,c(1,2,3)])) ##test plot
bjoint <- bjoint %>% select(-bcode)

sraster_joint <- left_join(ST_raster, bjoint)

ST_raster <- sraster_joint
ST_raster <- ST_raster %>% select(-bodenlands)
summary(ST_raster)

head(ST_raster)

ST_raster$ele_cat <- round_any(ST_raster$elevation, 100) ###round to nearest 100
ST_raster$slope <- round(ST_raster$slope)
ST_raster$slope_cat <- ifelse(ST_raster$slope<=5, 00, ifelse(ST_raster$slope %in% c(6:10), 01, ifelse(ST_raster$slope %in% c(11:20), 02, ifelse(ST_raster$slope %in% c(21:30), 03, ifelse(ST_raster$slope %in% c(31:40), 04, ifelse(ST_raster$slope %in% c(41:50), 05, ifelse(ST_raster$slope %in% c(51:60), 06, ifelse(ST_raster$slope %in% c(61:70), 07, ifelse(ST_raster$slope %in% c(71:80), 08, ifelse(ST_raster$slope %in% c(81:90), 09, ifelse(ST_raster$slope %in% c(91:100), 10, ifelse(ST_raster$slope %in% c(101:110), 11,12))))))))))))
ST_raster$aspect <- round(ST_raster$aspect)
ST_raster$asp_cat <- ifelse(ST_raster$aspect %in% c(0:22), 1, ifelse(ST_raster$aspect %in% c(23:68), 2, ifelse(ST_raster$aspect %in% c(68:112), 3, ifelse(ST_raster$aspect %in% c(113:157), 4, ifelse(ST_raster$aspect %in% c(158:202), 5, ifelse(ST_raster$aspect %in% c(203:247), 6, ifelse(ST_raster$aspect %in% c(248:292),7, ifelse(ST_raster$aspect %in% c(293:338),8, ifelse(ST_raster$aspect %in% c(339:360),1,0)))))))))


head(ST_raster)

for(t in 1:nrow(ST_raster)){
  ST_raster$cat_for[t] <- str_extract(ST_raster$tipi_for[t], "[A-z]+" )
}

writeRaster(ST_raster, paste0("C:/Users/semarzini/OneDrive - Scientific Network South Tyrol/Sebastian/Rprojects/REINFORCE/Dati aree studio/Venosta/stackVenosta_100m_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".tif"))






