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

wd <- setwd("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/VAL_VENOSTA/GIS")

dem <- raster("100m_DEM_20222403_1549.tif")
forest <- projectRaster(raster("tipiforestali_10m.tif"), dem, method = "ngb")
slope <- raster("100m_SLOPE_20222403_1549.tif")
aspect <- raster("100m_ASPECT_20222403_1549.tif")
depth <- raster("depth_100m.tif")
sand <- raster("sand_100m.tif")
silt <- raster("silt_100m.tif")
clay <- raster("clay_100m.tif")
bodenlands <- raster("bodenland_100m.tif")

depth <- projectRaster(depth, dem)
sand <- projectRaster(sand, dem)
silt <- projectRaster(silt, dem)
clay <- projectRaster(clay, dem)
bodenlands <- projectRaster(bodenlands, dem)

st <- raster::stack(forest, bodenlands, dem, slope, aspect, depth, sand, silt, clay)
plot(st)

d <- rasterToPoints(bodenlands)
dem_stack_df <- as.data.frame(d)

writeRaster(st, "stackVenosta_100m.tif", overwrite = TRUE)
