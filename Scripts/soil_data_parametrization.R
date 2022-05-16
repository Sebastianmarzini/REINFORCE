################################
## Stubaital Soil - K.Albrich ##
################################

#Companion file: Stubai_Dok_Soil_Prep.docx

#(1) some data prep 
#(2) estimating sand/silt/clay from "Waldtypisierung", collected by L. Oberhofer
#(3) estimating carbon and nitrogen pools from AFSS database
#(4) add pool of young refractory carbon based on AFI data
#(5) some testing and adjustement of n_available based on inventory plots
#(6) calculation of decomp rates and testing
#(7) creation of environment file
#(8) some further testing


library(raster)
library(rgdal)
library(sp)
library(plyr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(colorspace)
library(RSQLite)
library(tidyverse)
library(stringr)

#write.csv(soil_data, "soil_data_correct.csv")
#ru_raster <- raster("D:/Projects/RESIN/Stubaital/GISData/iLand_Input/raster_waldtypen.asc")
#ru_raster
#plot(ru_raster)
#head(ru_raster) 
#cellStats(ru_raster, range)
#ru <- rasterToPoints (ru_raster)
#head(ru)
#ru_df <- as.data.frame(ru)
#names(ru_df) <- c("x","y", "waldtyp")

###(1) Some data prep

###import DEM, previously calculated in arcmap

#wd <- setwd("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE")
dem_stack <- stack("stackVenosta_100m.tif") 
names(dem_stack) <- c("elevation","waldtyp", "slope", "aspect", "depth", "sand", "silt", "clay", "bodenlands")
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



###adjust slope, elevation and aspect to AFI categories (Schieler, 2001)

head(ST_raster)


#round
ST_raster$ele_cat <- round_any(ST_raster$elevation, 100) ###round to nearest 100
ST_raster$slope <- round(ST_raster$slope)
ST_raster$slope_cat <- ifelse(ST_raster$slope<=5, 00, ifelse(ST_raster$slope %in% c(6:10), 01, ifelse(ST_raster$slope %in% c(11:20), 02, ifelse(ST_raster$slope %in% c(21:30), 03, ifelse(ST_raster$slope %in% c(31:40), 04, ifelse(ST_raster$slope %in% c(41:50), 05, ifelse(ST_raster$slope %in% c(51:60), 06, ifelse(ST_raster$slope %in% c(61:70), 07, ifelse(ST_raster$slope %in% c(71:80), 08, ifelse(ST_raster$slope %in% c(81:90), 09, ifelse(ST_raster$slope %in% c(91:100), 10, ifelse(ST_raster$slope %in% c(101:110), 11,12))))))))))))
ST_raster$aspect <- round(ST_raster$aspect)
ST_raster$asp_cat <- ifelse(ST_raster$aspect %in% c(0:22), 1, ifelse(ST_raster$aspect %in% c(23:68), 2, ifelse(ST_raster$aspect %in% c(68:112), 3, ifelse(ST_raster$aspect %in% c(113:157), 4, ifelse(ST_raster$aspect %in% c(158:202), 5, ifelse(ST_raster$aspect %in% c(203:247), 6, ifelse(ST_raster$aspect %in% c(248:292),7, ifelse(ST_raster$aspect %in% c(293:338),8, ifelse(ST_raster$aspect %in% c(339:360),1,0)))))))))


head(ST_raster)

for(t in 1:nrow(ST_raster)){
  ST_raster$cat_for[t] <- str_extract(ST_raster$tipi_for[t], "[A-z]+" )
}

write.csv(ST_raster, "C:/Users/semarzini/Desktop/Sebastian/Rprojects/REINFORCE/Dati aree studio/Venosta/rasterStack_venosta.csv")

soil_data <- read.table("C:/Users/semarzini/Desktop/Sebastian/REINFORCE/Stubai/Dati Katharina Stubai/Stubai_soil_0802.csv", sep=",",header=TRUE)
#str(soil_data)

#2: estimating sand/silt/clay from "Waldtypisierung", collected by L. Oberhofer -----------------------------------------------------

#add forest types and match sand, silt, clay based on Lea's list

# ST_raster$waldtyp_descr <- soil_data$name_waldtyp[match(ST_raster$waldtyp, soil_data$waldtyp)]
# 
# ST_raster$sand <- soil_data$sand[match(ST_raster$waldtyp, soil_data$waldtyp)]
# ST_raster$silt <- soil_data$silt[match(ST_raster$waldtyp, soil_data$waldtyp)]
# ST_raster$clay <- soil_data$clay[match(ST_raster$waldtyp, soil_data$waldtyp)]
# 
# head(ST_raster)
# 
# #for soil depth, use values provided by forest types but introduce variation by sampling from a presumed normal distribution
# #depth and rock fraction are sampled separately, but are not entirely unrelated...
# 
# ##consider min/max as 3*sigma; calculate sd from them
# 
# soil_data$depth_sd <- (soil_data$depth_mean - soil_data$depth_min)/3
# soil_data$scele_sd <- (soil_data$scele_mean - soil_data$scele_min)/3
# 
# ### figure out how long sample vector for each type should be
# 
# n <- data.frame()
# waldtyp <- data.frame()
# 
# for (i in unique(ST_raster$waldtyp)){
#   
#   n_i <- nrow(ST_raster[ST_raster$waldtyp==i,])
#   waldtyp_i <- i
#   
#   n <- rbind (n, n_i)
#   waldtyp <- rbind (waldtyp, waldtyp_i)
#   
# }
# 
# n_needed <- data.frame(n=n[,1], waldtyp=waldtyp[,1])
# 
# soil_data$n <- n_needed$n[match(soil_data$waldtyp, n_needed$waldtyp)]
# 
# head(soil_data)
# 
# #sample soil depth
# 
# set.seed(1991)
# 
# samples_list <- list()
# 
# for (j in unique(soil_data$waldtyp)){
#   
#   
#   temp_depth <- (rnorm(soil_data$n[soil_data$waldtyp==j], mean=soil_data$depth_mean[soil_data$waldtyp==j], sd=soil_data$depth_sd[soil_data$waldtyp==j]))
#   
#   
#   name <- paste("depth_sample",j,sep="")
#   samples_list[[name]] <- temp_depth
#   
# }    
# 
# ###add soil depth column 
# 
# ST_raster <- arrange(ST_raster, waldtyp)
# 
# samples <- c(unlist(samples_list$depth_sample1), unlist(samples_list$depth_sample2), unlist(samples_list$depth_sample3), unlist(samples_list$depth_sample4),unlist(samples_list$depth_sample5), unlist(samples_list$depth_sample6), unlist(samples_list$depth_sample7), unlist(samples_list$depth_sample8), unlist(samples_list$depth_sample9), unlist(samples_list$depth_sample10), unlist(samples_list$depth_sample11), unlist(samples_list$depth_sample12), unlist(samples_list$depth_sample13), unlist(samples_list$depth_sample14), unlist(samples_list$depth_sample15), unlist(samples_list$depth_sample16))
# 
# 
# ST_raster$soil_depth <- samples ###risky way of doing that
# 
# head(ST_raster)
# 
# ###same procedure for rock fraction
# 
# set.seed(1991)
# 
# scele_samples_list <- list()
# 
# for (j in unique(soil_data$waldtyp)){
#   
#   
#   temp_scele <- (rnorm(soil_data$n[soil_data$waldtyp==j], mean=soil_data$scele_mean[soil_data$waldtyp==j], sd=soil_data$scele_sd[soil_data$waldtyp==j]))
#   
#   
#   name <- paste("scele_sample",j,sep="")
#   scele_samples_list[[name]] <- temp_scele
#   
# }    
# 
# ###add soil depth column 
# 
# 
# scele_samples <- c(unlist(scele_samples_list$scele_sample1), unlist(scele_samples_list$scele_sample2), unlist(scele_samples_list$scele_sample3), unlist(scele_samples_list$scele_sample4),unlist(scele_samples_list$scele_sample5), unlist(scele_samples_list$scele_sample6), unlist(scele_samples_list$scele_sample7), unlist(scele_samples_list$scele_sample8), unlist(scele_samples_list$scele_sample9), unlist(scele_samples_list$scele_sample10), unlist(scele_samples_list$scele_sample11), unlist(scele_samples_list$scele_sample12), unlist(scele_samples_list$scele_sample13), unlist(scele_samples_list$scele_sample14), unlist(scele_samples_list$scele_sample15), unlist(scele_samples_list$scele_sample16))
# 
# 
# ST_raster$soil_scele <- scele_samples
# 
# #no calculate effective soil depth
# 
# ST_raster <- mutate(ST_raster, effective_depth=soil_depth*(1-soil_scele/100))
# head(ST_raster)



#3: estimating carbon and nitrogen pools from AFSS database ------------------------------------------------------------------------

#load soil data from AFI

AFI_databank <- read.table("C:/Users/semarzini/Desktop/Sebastian/REINFORCE/Stubai/Dati Katharina Stubai/AFI_soil_data_simple.csv", sep=";", header=TRUE)
AFI_databank_filterd <- AFI_databank %>% filter(WUGEB==1.2 | WUGEB==1.1 | WUGEB==3.3 ) #take only plots from the relevant zone

#make subgroups of soils according to forest type by selecting only the soil types mentioned in the description

head(AFI_databank_filterd)

AFI_AE <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde"))
AFI_AS <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde"))
AFI_AT <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde"))
AFI_Ei9 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Kalk-Braunlehm", "reiche Breunerde"))
AFI_EK3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Semipodsol", "arme Braunerde", "Rendsina"))
AFI_EK4 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "arme Braunerde"))
AFI_EK5 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Semipodsol", "arme Braunerde", "Rendsina"))
AFI_EK6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Semipodsol", "arme Braunerde", "Rendsina"))
AFI_Fi1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "reiche Braunerde", "Lockersedimentbraunerde"))
AFI_Fi2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Podsol", "Semipodsol"))
AFI_Fi3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde"))
AFI_Fi4 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde"))
AFI_Fi5 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Lockersedimentbraunerde" ))
AFI_Fi6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Lockersedimentbraunerde"))
AFI_Fi7 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Lockersedimentbraunerde"))
AFI_Fi8 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Lockersedimentbraunerde"))
AFI_Fi15 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "Semipodsol"))
AFI_Fs1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "Semipodsol"))
AFI_Fs2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "Semipodsol"))
AFI_Fs3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "Semipodsol", "Podsol"))
AFI_Fs4 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "Semipodsol"))
AFI_Fs5 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde", "Semipodsol", "Kalk-Braunlehm"))
AFI_Fs6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "Kalk-Braunlehm", "reiche Braunerde"))
AFI_Fs7 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "Kalk-Braunlehm", "reiche Braunerde"))
AFI_Fs8 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "Kalk-Braunlehm", "reiche Braunerde"))
AFI_Fs9 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde", "Semipodsol", "Kalk-Braunlehm"))
AFI_Fs10 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Podsol", "Semipodsol, Lockersedimentbraunerde"))
AFI_Fs15 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "Semipodsol"))
AFI_FT1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde"))
AFI_FT5 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Podsol", "Semipodsol"))
AFI_FT6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Kalk-Braunlehm", "Semipodsol"))
AFI_FT11 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde"))
AFI_FT12 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Podsol", "Semipodsol"))
AFI_FT14 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "reiche Braunerde"))
AFI_FT15 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Kalk-Braunlehm", "Semipodsol"))
AFI_FT16 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Kalk-Braunlehm", "Semipodsol"))
AFI_FT19 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Kalk-Braunlehm", "Semipodsol"))
AFI_Ge1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde"))
AFI_Ki1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde", "Rendsina", "Kalk-Braunlehm"))
AFI_Ki6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde", "Semipodsol", "arme Braunerde", "Lockersedimentbraunerde"))
AFI_Ki8 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde", "Semipodsol", "arme Braunerde", "Lockersedimentbraunerde"))
AFI_La2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "Kalk-Braunlehm"))
AFI_La3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Semipodsol", "reiche Braunerde"))
AFI_La6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde", "Semipodsol", "Podsol"))
AFI_La8 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde"))
AFI_La9 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "Podsol"))
AFI_Lat1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "Kalk-Braunlehm", "reiche Braunerde"))
AFI_Lat2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "Kalk-Braunlehm", "reiche Braunerde"))
AFI_Lat3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Podsol", "Lockersedimentbraunerde"))
AFI_Lh15 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde", "reiche Braunerde"))
AFI_Zi1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Semipodsol", "Podsol", "Lockersedimentbraunerde"))
AFI_Zi2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
AFI_Zi3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Podsol", "arme Braunerde"))
AFI_Zi4 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Semipodsol", "arme Braunerde", "Lockersedimentbraunerde"))
AFI_Zi6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Kalk-Braunlehm"))
AFI_Zi7 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
AFI_nf <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Sonstige"))


# AFI_HS <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde"))
# AFI_TR <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina"))
# AFI_AGh_EWIAMA_o2200 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde"))
# AFI_AGh_EWUCUG_o2200 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("arme Braunerde"))
# AFI_AGhUundiff <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina"))
# AFI_AGhMB_IU <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde", "Kalk-Braunlehm"))
# AFI_EWUF <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde"))
# AFI_AGhUF <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde"))
# AFI_AGh_EWIAMA_u2200 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde", "arme Braunerde", "Podsol"))
# AFI_AGh_EWUCUG_u2200 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde", "arme Braunerde", "Podsol"))
# AFI_EWMB_IU <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina"))
# AFI_ZSGIA-MA <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde", "Podsol"))
# AFI_EWUGGB <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina"))
# AFI_ZSGUGGB <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde", "Podsol"))
# AFI_ZSGUCUG <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Lockersedimentbraunerde", "Podsol"))
# AFI_EWUundiff <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina"))
# AFI_ZSGUundiff <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde", "arme Braunerde", "Podsol"))
# AFI_ZSGUF <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("reiche Braunerde"))
# AFI_Ki6_8La3MH3_4_6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Fs10-FT5La6Ge1Zi1_4 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Fi1Fi3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Fi4FT12 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_MW <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Acker <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Ei2_5Fs1_2_3_4_15FT1_11Ftb3_4Bu2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Fi5_6_7Fs6_7_8Ki1La2Zi2_7 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_AGhUGGB <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Fs5_9FT6_15_16_19Ftb1_Zi6 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Ei9_11EK4La8 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_EK1_2_3_5_6La9Zi3 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_GwnB1 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Ftb11_15Bu6FT14Lh11_15 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_GwfB2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_GwnB2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_ZSGMB_IU <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Ki7Ki13 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Fi8_15Lat1_2 <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_GwfB <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Gletscher <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_GwnB <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_Moore <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))
# AFI_See <- filter(AFI_databank_filterd, BOGRUP_WBZ %in% c("Rendsina", "reiche Braunerde", "Kalk-Braunlehm"))

ST_raster_filter <- ST_raster %>% filter(tipi_for=="nf")

ST_raster$tipi_for <- as.character(ST_raster$tipi_for)
ST_raster$xy <- paste(ST_raster$x, ST_raster$y, sep="")


###selecting matching sample plots according to aspect, slope and elevation, based on Code by Dominik

select.soil <- function (soil_database_select, elevation, slope, aspect) {
  d <- soil_database_select$ID[soil_database_select$Seehoehe==elevation &  soil_database_select$NEIG==slope & soil_database_select$EXP==aspect]
  if (length(d)==0) {
    print("removing aspect")
    d <- soil_database_select$ID[soil_database_select$Seehoehe==elevation & soil_database_select$NEIG==slope]}
  else{(print(d))}
  
  if (length(d)==0) {
    print("removing slope")
    d <- soil_database_select$ID[soil_database_select$Seehoehe==elevation]}
  else{(print(d))}
  
  if (length(d)==0) {
    print("removing elevation")
    d <- soil_database_select$ID}
  return(d)
}

sampleWithoutSurprises <- function(x) {
  if (length(x)<=1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}

soil_q <- data.frame()
soil_all <- data.frame()


for (i in (unique(ST_raster$tipi_for))){
  soil_sele <- get(paste("AFI_",i, sep=""))
  point_sele <- ST_raster[ST_raster$tipi_for==i,]
  

  for (q in (c(1:nrow(point_sele)))) {
    aspect <- point_sele[q,"asp_cat"]
    elevation <- point_sele[q,"ele_cat"]
    slope <- point_sele[q,"slope_cat"]
    xyc <- point_sele[q, "xy"]
    
    g <- select.soil(soil_sele, elevation, slope, aspect)
    
    ID_q <- sampleWithoutSurprises(g)
    
    soil_q <- soil_sele[soil_sele$ID==ID_q,]
    soil_q$tipi_for <- i
    soil_q$xy <- xyc
    soil_all <- rbind(soil_all, soil_q) 
    
}
  

}


head(soil_all)
unique(soil_all$waldtyp_descr)
nrow(soil_all[soil_all$waldtyp_descr=="Fs1",])


soil_raster <- merge(x=ST_raster, y=soil_all, by="xy", all=TRUE)

for(i in 1:nrow(soil_raster)){
  if(soil_raster[i,]$depth == 0){
    soil_raster[i,][,16:66] <- 0
  }
}

soil_raster <- soil_raster %>% rename(tipi_for = tipi_for.x)

write.csv(soil_raster, paste0("VAL_VENOSTA/soil_venosta_", Sys.Date(), ".csv"), row.names = FALSE )

#4: add pool of young refractory carbon based on AFI data ----------------------------------------------------------------------------

soil_raster <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/VAL_VENOSTA/soil_venosta_2022-04-12.csv")

infc_punti <- as.data.frame(readOGR("ITALY_NFI_infc05/shape/infc_AltoAdige.shp"))
infc.sel <- infc_punti %>% select(idpunto, LAT_ND_W84, LON_ND_W84, elevation, aspect, slope, WTYP_CODE, cffddesc1, Napm_ha, Gapm_ha, Vapm_ha, Wapm_ha,
                            Capm_ha, Nce_ha, Vce_ha, Wce_ha, Cce_ha, Vne_ha, Wne_ha, Cne_ha)

infc_data2 <- read.csv("ITALY_NFI_infc05/Dati per punto inventariale/Dati lettiera necromassa fine e suolo - infc05_quantiF3+_1/t1_05_quantiF3+.csv", sep = ";")
infc_data2 <- infc_data2 %>% rename(ID = idpunto)

infc_ok <- left_join(infc.sel, infc_data2)

deadwood <- read.xlsx("D:/Projects/RESIN/Stubaital/Bodendaten/deadwood_tyrol.xlsx",1) ##exported from AFI website, waldinventur.at
deadwood$ele_cat <- c(1,2,3,4,5,6,NA)
head(deadwood)

soil_raster$ele_cwd <- ifelse(soil_raster$elevation<300, 1, 
                              ifelse(soil_raster$elevation %in% c(300:599), 2, 
                                     ifelse(soil_raster$elevation %in% c(600:899), 3,
                                            ifelse(soil_raster$elevation %in% c(900:1199), 4, 
                                                   ifelse(soil_raster$elevation %in% c(1200:1499), 5,
                                                          ifelse(soil_raster$elevation>1499, 6, NA))))))
head(soil_raster)

#prova <- soil_raster %>% select(elevation, ele_cwd)

n <- data.frame()
ele_cat <- data.frame()

for (i in unique(soil_raster$ele_cwd)){
  
  n_i <- nrow(soil_raster[soil_raster$ele_cwd==i,])
  ele_cat_i <- i
  
  n <- rbind (n, n_i)
  ele_cat <- rbind (ele_cat, ele_cat_i)
  
}

n_needed <- data.frame(n=n[,1], ele_cat=ele_cat[,1])
head(n_needed)

deadwood <- left_join(deadwood, n_needed)
deadwood <- deadwood[c(3:6),]

set.seed(1991)

samples_list <- list()

for (j in unique(deadwood$ele_cat)){
  
  
  standingdead <- (rnorm(deadwood$n[deadwood$ele_cat==j], mean=deadwood$mean_vol_m3[deadwood$ele_cat==j], sd=deadwood$sd_vol_m3[deadwood$ele_cat==j]))
  
  
  name <- paste("dead_sample",j,sep="")
  
  samples_list[[name]] <- standingdead
  
}    

#turn into a vector to attach to dataframe
standing_dead <- c(unlist(samples_list$dead_sample3),unlist(samples_list$dead_sample4),unlist(samples_list$dead_sample5),unlist(samples_list$dead_sample6) )
standing_dead[1]<- 4.4 #assign actual mean as there is only one plot from that group
##add 

mean(samples_list$dead_sample6) ##check means of new distribution

soil_raster <- soil_raster[order(soil_raster$ele_cwd),]
soil_raster$snags <- standing_dead ##again lazy

mean(soil_raster$snags[soil_raster$ele_cwd==6]) ##check if sorting worked and distributions are still okay

#go from standing to lying, constant ratio derived from BFW Praxisinfo

soil_raster$dwd <- soil_raster$snags*(deadwood$ratio_lying_standing[1]) #apply ratio over all elevations to get cwd

mean(soil_raster$dwd)
#check mean for dwd overall 14.81 - still reasonably close to 14.62 as overall value for Innenalpen in 2007-9 (snag values in 2001-2 are lower but here we have only the highest elevations, where the values are closer to/higher than 2007-2009 overall value)

soil_raster$dwd_C <- soil_raster$dwd*250 
#(deadwood*1000**0.5*0.5), (1000 to get Kg ha-1, 0.5 for the dry weight and 0.5 for the carbon equivalent; see also Dominik, NP

soil_raster$snag_C <- soil_raster$snags*250
soil_raster$deadwood_C <- soil_raster$dwd_C+soil_raster$snag_C

#Discussion: LitterC and deadwoodC are only "aboveground" but iLand pools also include roots. Needs to be kept in mind for comparison

#5: some testing and adjustement of n_available based on inventory plots ----------------------------------------------------------------


plot(rasterFromXYZ(soil_raster[,c(2,3,17)]), col=my_col) ##plot effective soil depth
plot(rasterFromXYZ(soil_raster[,c(2,3,30)]), col=my_col) ##plot n_available
plot(rasterFromXYZ(soil_raster[,c(2,3,28)])) ##SOMC
plot(rasterFromXYZ(soil_raster[,c(2,3,26)])) ##LitterC
plot(rasterFromXYZ(soil_raster[,c(2,3,5)])) ##elevation
plot(rasterFromXYZ(soil_raster[,c(2,3,7)])) ##aspect
plot(rasterFromXYZ(soil_raster[,c(2,3,6)])) ##slope
plot(rasterFromXYZ(soil_raster[,c(2,3,14)]), zlim=c(0,100), col=my_col, main="Clay") ##clay
plot(rasterFromXYZ(soil_raster[,c(2,3,74)]), col=my_col)


###merge to ru id, prepare rest of environment

#import resource unit raster
ru_id_raster <- raster("D:/Projects/RESIN/Stubaital/Bodendaten/iland.ru.id.asc")


ru_id_df <- as.data.frame(rasterToPoints(ru_id_raster))
names(ru_id_df) <- c("x", "y", "ru.id")
ru_id_df$xy <- paste(ru_id_df$x, ru_id_df$y, sep="")
head(ru_id_df)


env <- merge(x=ru_id_df, y=soil_raster, by="xy", all=TRUE)
env <- env[,c(1,4:ncol(env))]
names(env)[3] <- "x"
names(env)[4] <- "y"

plot(rasterFromXYZ(env[,c(3,4,2)])) 

##add climate

climate_raster <- raster("D:/Projects/RESIN/Stubaital/Klimadaten/cluster.map.asc")
climate_raster_df <- as.data.frame(rasterToPoints(climate_raster))
names(climate_raster_df) <- c("x", "y", "climate.cluster")
climate_raster_df$xy <- paste(climate_raster_df$x, climate_raster_df$y, sep="")


plot(climate_raster)
head(climate_raster_df)
env <- merge(x=climate_raster_df, y=env, by="xy", all=TRUE)
env <- env[,c(1,4:ncol(env))]
names(env)[4] <- "x"
names(env)[5] <- "y"


env_order <- env[order(env$ru.id),]

env_order$SOMC_kg <- env_order$SOMC*1000
env_order$LitterC_kg <- env_order$LitterC*1000
env_order$SOMN_kg <- env_order$SOMN*1000
env_order$LitterN_kg <- env_order$LitterN*1000
env_order$total_C <- env_order$SOMC_kg + env_order$LitterC_kg + env_order$deadwood_C


# some more test plots 
my_col <- colorRampPalette(brewer.pal(9, "PuBuGn"))(10)
my_col <- rev(terrain_hcl(10))
plot(rasterFromXYZ(env_order[,c(4,5,13)]), col=my_col)
plot(rasterFromXYZ(env_order[,c(4,5,32)]), col=my_col)

#adjustement of n_available after comparison
#adjust n_avalailable for southern slope, lower elevations, forest types Fi3, Fi4, FT1 where productivity is too high (see volume_obs_sim.pdf, basalarea_obs_sim.pdf)

head(env_order)

###add categories according to wzp data for easier comparison

env_order <- env_order %>%
        mutate(ele_cat_wzp=cut(elevation,breaks=c(800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400), labels=c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)))

env_order$asp_cat_wzp <- env_order$asp_cat4 <- ifelse(env_order$aspect %in% c(0:44), 1, ifelse(env_order$aspect %in% c(45:134), 2, ifelse(env_order$aspect %in% c(135:224), 3, ifelse(env_order$aspect %in% c(225:314), 4, ifelse(env_order$aspect %in% c(315:360), 1,0)))))

###quick overview of current nitrogen distribution
summary(env_order$N_available)


ggplot(env_order, aes(y=N_available, x=ele_cat_wzp, group=ele_cat_wzp)) + geom_boxplot()+ theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text=element_text(size=20)) + ylab("plant available nitrogen (kg/ha/year)") + xlab("elevation_category") +coord_cartesian(ylim=c(25,110))

ggplot(env_order, aes(y=N_available, x=asp_cat_wzp, group=asp_cat_wzp)) + geom_boxplot()+ theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text=element_text(size=20)) + ylab("plant available nitrogen (kg/ha/year)") + xlab("aspect_category") +coord_cartesian(ylim=c(25,110))

my_col <- colorRampPalette(brewer.pal(9, "PuBuGn"))(10)
plot(rasterFromXYZ(env_order[,c(4,5,85)]), col=my_col, zlim=c(0,100))

###highest in elevation categories 10-15 and on southern slopes, exactly where we also overestimate the most
###further investigation of these plots

env_order_sl <- env_order[env_order$asp_cat_wzp %in% c(2,3) & env_order$ele_cat_wzp %in% c(9,10,11,12,13,14,15),]
summary(env_order_sl)
unique(env_order_sl$waldtyp_descr.x)
quantile(env_order_sl$N_available, 0.75) #74.95832
cut_quant <- 74.95832 
cut_quant <- quantile(env_order$N_available, 0.95) #84.22384
cut_median <- median(env_order_sl$N_available) #71.21708

#as a first try, cut the distribution at the 90th percentile for the stands in question
#env_order$N_available_new <- ifelse(env_order$asp_cat_wzp %in% c(2,3) & env_order$ele_cat_wzp %in% c(9,10,11,12,13,14,15) & env_order$N_available>cut_quant,cut_quant,env_order$N_available)

##one last try, global cut

env_order$N_available_new <- ifelse(env_order$N_available>84.22384,84.22384,ifelse(env_order$asp_cat_wzp %in% c(2,3) & env_order$ele_cat_wzp %in% c(9,10,11,12,13,14,15) & env_order$N_available>71.21708, env_order$N_available-(env_order$N_available/20), env_order$N_available))

summary(env_order$N_available_new)

###new nitrogen distribution over elevation and aspect

ggplot(env_order, aes(y=N_available_new, x=ele_cat_wzp, group=ele_cat_wzp)) + geom_boxplot()+ theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text=element_text(size=20)) + ylab("plant available nitrogen (kg/ha)") + xlab("elevation_category") +coord_cartesian(ylim=c(25,110))

ggplot(env_order, aes(y=N_available_new, x=asp_cat_wzp, group=asp_cat_wzp)) + geom_boxplot()+ theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"), text=element_text(size=20)) + ylab("plant available nitrogen (kg/ha)") + xlab("aspect_category") +coord_cartesian(ylim=c(25,110))

my_col <- colorRampPalette(brewer.pal(9, "PuBuGn"))(10)
plot(rasterFromXYZ(env_order[,c(4,5,85)]), col=my_col)


#(6) calculation of decomp rates and testing

##reference pools of litter and dwd need to be adjusted first, as iLand definition also includes roots, branches left behind by management, which are not included in inventory data


db.conn <- dbConnect(RSQLite::SQLite(), dbname="D:/Projects/RESIN/Stubaital/Stubaital_Sim/output/output_spin_plantred2.sqlite")
soilinput <- dbReadTable(db.conn, "soilinput")
landscape <- dbReadTable(db.conn, "landscape")
carbon <- dbReadTable(db.conn, "carbon")
carbonflow <- dbReadTable(db.conn, "carbonflow")
dbDisconnect(db.conn)

#calculate species spare for adjusting snag halflive (volume share) and litter decomp (LAI share)
head(landscape)
spec_share <- landscape %>%
  group_by(species) %>%
  summarize(LAI=mean(LAI), vol=mean(volume_m3)) %>%
  mutate(share_LAI=LAI/sum(LAI), share_vol=vol/sum(vol)) %>%
  mutate(share_LAI_percent=round(.$share_LAI*100,2), share_vol_percent=round(.$share_vol*100,2))

#vol: piab 95 %, lade 3 %, pice 2 %
#LAI: piab 92 %, lade+pice 8 %  lade 4 %, pice 4 %


#fraction of snag pool entering dwd - multiply with species ratios

#1-exp(-log(2)/30) #piab
#1-exp(-log(2)/40) lade,

##get input from snags, fine roots

carbon_avg <- carbon %>%
  filter(rid==-1) %>%
  summarise_all(.funs=mean) %>%
  mutate(root_shoot=(coarseRoot_c)/(stem_c),#
         stem_branch=(branch_c)/(stem_c),
         snag_input=(snags_c*0.95*(1-exp(-log(2)/30))+snags_c*0.05*(1-exp(-log(2)/40))),
         fine_root_share=foliage_c*0.75*1/(foliage_c*0.92*0.114025086
+foliage_c*0.04*1+foliage_c*0.04*0.409836066
)) %>%
  mutate(snag_input_t=snag_input/1000)
   

##input of branches remaining after mgmt

carbon_mgmt_loss <- carbonflow %>%
  filter(rid==-1) %>%
  summarise(mean_c=mean(mgmt_loss)) %>% 
  mutate(branch_input=(mean_c*carbon_avg$stem_branch)/1000*(-1))
         


###load soilinput from spinup



soilinput <- soilinput[!soilinput$rid==0,] ###for lab and ref no grouping and stratifying, all on landscape level as it goes into tree species params

#adjust reference pools

soilinput_avg <- soilinput %>%
  #group_by(rid) %>%
  summarise(input_lab=mean(input_lab),
            input_ref=mean(input_ref),
            re=mean(re)) %>%
  mutate(input_ref_root=input_ref-(carbon_mgmt_loss$branch+carbon_avg$snag_input_t)) %>%
  mutate(input_ref_ratio=(input_ref-(carbon_avg$snag_input_t+carbon_mgmt_loss$branch_input/1000))/input_ref)

reference_pools <- env_order %>%
  summarise(reference_lab=mean(LitterC),
  reference_ref=mean(dwd_C/1000)) %>%
  mutate(reference_lab_all=reference_lab+reference_lab*carbon_avg$fine_root_share,
  reference_ref_all=reference_ref+reference_ref/(1-soilinput_avg$input_ref_ratio)*(soilinput_avg$input_ref_ratio))


### stratify on the level of forest types

 # decomprates_strat <-decomprates %>%
 #   group_by(waldtyp_descr.x) %>%
 #   summarize(SOMC=mean(SOMC),
 #             LitterC=mean(LitterC),
 #             dwdC=mean((dwd_C+carbon_mgmt_loss$root)/1000),
 #             input_lab=mean(input_lab),
 #             input_ref=mean(input_ref),
 #             re=mean(re))

##Step 1 - decay rates under given climate

##youngLabileDecompRate=input_lab/reference_lab
##youngRefractoryDecompRate=input_ref/reference_ref
decomp <- soilinput_avg %>%
  mutate(youngLabile_cur=input_lab/reference_pools$reference_lab_all,
         youngRefractory_cur=input_ref/reference_pools$reference_ref_all)

##Step 2 - decomp rates under standardised climate 
##youngLabileDecompRate=youngLabileDecompRate/re
##youngRefractoryDecompRate=youngRefractoryDecompRate

decomp <- decomp %>%
  mutate(youngLabile_stand=youngLabile_cur/re,
         youngRefractory_stand=youngRefractory_cur/re)


##Step 3 - som DecompRate. somDecompRate=(input_lab+input_ref)*h/reference_SOM*re As a first step, I assume a generic h of 0.25 (based on Xenakis et. al boundaries of 0.1-0.4)

som_decomp <- decomprates_strat %>%
    mutate(somDecompRate=((input_lab+input_ref)*0.25)/(SOMC*re))
  

glimpse(decomp)


env_final <- left_join(env_order, som_decomp, by=c("waldtyp_descr.x"))

head(env_final)
env_final$litterC_new_kg <- env_final$LitterC_kg + env_final$LitterC_kg*carbon_avg$fine_root_share
env_final$dwd_C_new_kg <- env_final$dwd_C+env_final$dwd_C/(1-soilinput_avg$input_ref_ratio)*(soilinput_avg$input_ref_ratio)



#(7) creation of environment file


environment_file <- data.frame(id=env_final$ru.id, model.climate.tableName=paste("climate",env_final$climate.cluster, sep=""), model.site.availableNitrogen=env_final$N_available_new, model.site.soilDepth=env_final$effective_depth, model.site.pctSand=env_final$sand, model.site.pctSilt=env_final$silt, model.site.pctClay=env_final$clay,model.site.youngLabileC=env_final$litterC_new_kg, model.site.youngLabileN=env_final$LitterN_kg, model.site.somC=env_final$SOMC_kg, model.site.somN=env_final$SOMN_kg, model.site.youngRefractoryC=env_final$dwd_C_new_kg, model.site.somDecompRate=env_final$somDecompRate, model.site.soilHumificationRate=0.25)

environment_file[is.na(environment_file$model.site.somDecompRate),]

write.table(environment_file, "Stubai_env_decompRates.txt", row.names=FALSE, sep=" ")

#(8) some further testing regarding carbon pools



db.conn <- dbConnect(RSQLite::SQLite(), dbname="D:/Projects/RESIN/Stubaital/Stubaital_Sim/output/output_spin_plantred2.sqlite")
carbon_new <- dbReadTable(db.conn, "carbon")
carbonflow_new <- dbReadTable(db.conn, "carbonflow")
dbDisconnect(db.conn)

par(mfrow=c(2,2))
par(cex=1.3)
plot(carbon_new$soil_c[carbon_new$rid==-1]/1000, type="l", ylim=c(0,200), xlab="year", main="Soil carbon", ylab="[t/ha]")
abline(mean(env_order$SOMC_kg)/1000,0)
plot(carbon_new$litter_c[carbon_new$rid==-1]/1000, type="l", ylim=c(0,200), xlab="year", ylab="[t/ha]", main="Young labile carbon ")
abline(reference_pools$reference_lab_all,0)
plot(carbon_new$downedWood_c[carbon_new$rid==-1]/1000, type="l", ylim=c(0,200), xlab="year", ylab="[t/ha]", main="Young refractory carbon")
abline(reference_pools$reference_ref_all,0)
plot(carbon_new$stem_c[carbon_new$rid==-1]/1000, type="l", xlab="year", ylim=c(0,200), ylab="[t/ha]", main="Stem carbon")




db.conn <- dbConnect(RSQLite::SQLite(), dbname="D:/Projects/RESIN/Stubaital/Stubaital_Sim/output/output_ntest.sqlite")
carbonflow <- dbReadTable(db.conn, "carbonflow")
dbDisconnect(db.conn)

db.conn <- dbConnect(RSQLite::SQLite(), dbname="D:/Projects/RESIN/Stubaital/Stubaital_Sim/output/output_spin_plantred2.sqlite")
carbon <- dbReadTable(db.conn, "carbon")
carbonflow <- dbReadTable(db.conn, "carbonflow")
dbDisconnect(db.conn)

db.conn <- dbConnect(RSQLite::SQLite(), dbname="D:/Projects/RESIN/Stubaital/Stubaital_Sim/output/output_nomgmt_bl.sqlite")
carbon_new <- dbReadTable(db.conn, "carbon")
dbDisconnect(db.conn)

db.conn <- dbConnect(RSQLite::SQLite(), dbname="D:/Projects/RESIN/Stubaital/Stubaital_Sim/output/snap_soil.sqlite")
snags <- dbReadTable(db.conn, "snag")
soil <- dbReadTable(db.conn, "soil")
carbon <- dbReadTable(db.conn, "carbon")
dbDisconnect(db.conn)

soilinput_avg_year <- soilinput %>%
  group_by(year) %>%
  summarise_all(.funs = mean)



carbon_clean <- carbon[!carbon$rid %in% c(0,-1),]


tail(environment_file)

carbon_plansim <- left_join(carbon_clean, environment_file, by=c("rid"="id"))


carbon_landsc <- carbon %>%
  group_by(year) %>%
  summarise(stem_c=mean(stem_c),
            branch_c=mean(branch_c),
            foliage_c=mean(foliage_c),
            coarseRoot_c=mean(coarseRoot_c),
            fineRoot_c=mean(fineRoot_c),
            snags_c=mean(snags_c),
            snagsOther_c=mean(snagsOther_c),
            downedWood_c=mean(downedWood_c),
            litter_c=mean(litter_c),
            soil_c=mean(soil_c))
            
  

plot(carbon_landsc$soil_c, ylim=c(0,200000), type="l")
lines(carbon_landsc$stem_c, col="brown")
lines(carbon_landsc$coarseRoot_c, col="blue")
lines(carbon_landsc$fineRoot_c, col="lightblue")
lines(carbon_landsc$downedWood_c, col="red")
lines(carbon_landsc$litter_c, col="pink")
lines(carbon_landsc$snags_c, col="darkgrey")
lines(carbon_landsc$snagsOther_c, col="grey")

plot(carbon_plansim$soil_c[carbon_plansim$rid==1000], type="l")
abline(carbon_plansim$model.site.somC[carbon_plansim$rid==1000],0)


plot(carbon_plansim$litter_c[carbon_plansim$rid==100], type="l")
lines(carbon_plansim$model.site.youngLabileC[carbon_plansim$rid==100])

plot(carbon_plansim$downedWood_c[carbon_plansim$rid==1], type="l")
lines(carbon_plansim$model.site.youngRefractoryC[carbon_plansim$rid==1])






carbonflow <- carbonflow[!carbonflow$rid==0,]
summary(carbonflow[carbonflow$year==5,])



snags <- snags[!snags$RUIndex==0,]
#soil <- soil[!soil$RUindex==0,]

soil_comp <- left_join(environment_file, carbonflow, by=c("id"="rid"))
soil_comp <- left_join(soil, soil_comp, by=c("RUindex"="ru"))
soil_comp <- soil_comp[!is.na(soil_comp$id),]
summary(soil_comp)



###############old code


# write.xlsx(env, "Stubai_environment1.xlsx", row.names=FALSE)
# 
# 
# ###make empty tree/empty saplings files
# 
# stand_id_raster <- raster("D:/Projects/RESIN/Stubaital/GISData/Vegetation_clean/stand_ids.tif")
# 
# stand_id_rastoPoint <- as.data.frame(rasterToPoints(stand_id_raster))
# names(stand_id_rastoPoint) <- c("x", "y", "stand_id")
# stand_id <- unique(stand_id_rastoPoint$stand_id)
# 
# empty_trees <- data.frame("stand_id"=stand_id, "species"="piab", "count"=0, "dbh_from"=NA, "dbh_to"=NA, "hd"=NA, "age"=NA)
# empty_saplings <- data.frame("stand_id"=stand_id, "species"="piab", "count"=0, "dbh_from"=NA, "dbh_to"=NA, "hd"=NA, "age"=NA)
# bare_soil <- data.frame("species"="", "count"="", "dbh_from"="", "dbh_to"="", "hd"="", "age"="", "density"="")
# 
# write.csv(empty_trees, "empty_trees.csv", row.names=FALSE)
# write.csv(empty_saplings, "empty_saplings.csv", row.names=FALSE)



##old kyr 0.09936559


# <- env_order[c(2:9, 13:19, 85)]
#write.csv(short_env, "environment_short.csv")


#head(short_env)

#short_env <- dplyr::rename(short_env, "rid"="ru.id", "elevation_dem_%"="elevation", "slope_dem_deg"="slope", "aspect_dem"="aspect", "waldtyp_descr"="waldtyp_descr.x", "n_available"="N_available_new")

#plot(rasterFromXYZ(short_env[,c(3,4,16)]))


#write.csv(short_env, "forest_types_rid.csv")     
     