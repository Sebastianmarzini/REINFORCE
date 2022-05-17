#05/05/2022
# Elaborazione dati INFC necromassa alto adige da associare all'area di studio in base alla tipologia forestale, l'altitudine,
# la pendenza e l'esposizione

rm(list=ls())

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
library(tibble)

# 1 Sistemazione dati INFC ----

# csv con dati relativi all'area di studio
#soil_raster <- read.csv("C:/Users/seba_/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/VAL_VENOSTA/soil_venosta_2022-04-12.csv")

# dati INFC necromassa fine e suolo con coordinate giuste
nfs <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/infc necromassa e tipi forestali_altoadige.csv", fileEncoding="UTF-8-BOM")

# tolgo boschi igrofili e colonne che non servono
nfs <- nfs %>% filter(WTYP_CODE!="AT") %>% 
  dplyr::select(-c(Wnef_ha, cffddesc1, COD_FOR))

# dati INFC necromassa popolamento
pn <- read.csv("C:/Users/seba_/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/infc popolamento e necromassa alto adige e tipi forestali con coordinate_2022_05_05.csv", fileEncoding="UTF-8-BOM")

infc <- left_join(pn, nfs, by = "idpunto")
infc <- infc %>% dplyr::select(-c(WTYP_CODE.y, WTYP_CODE.x))

# creo una nuova colonna per i gruppi forestali: utilizzo il valore del gruppo del secondo dataframe, se ? NA uso quello del primo
for(i in 1:nrow(infc)){
  infc$WGRU_CODE[i] <- ifelse(is.na(infc$WGRU_CODE.y[i]) == FALSE, infc$WGRU_CODE.y[i], infc$WGRU_CODE.x[i])
}

infc <- infc %>% select(-c(WGRU_CODE.x, WGRU_CODE.y, LAT_WGS84, LON_WGS84))

#write.csv(infc, paste0("INFC data South Tyrol/dati utili INFC alto adige_", Sys.Date(), ".csv"), row.names = FALSE)


# 2 Creazione di classi per elevazione, slope e aspect ----

#data <- read.csv("INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_2022-05-10_.csv")

# classi altitudinali come quelle nel file di deadwood di Katarina per lo Stubai pi? altre classi sopra i 1500 m: 
data$elevation <- round(data$elevation)
data$ele_cat <- ifelse(data$elevation<300,1, 
                       ifelse(data$elevation %in% c(300:599), 2, 
                              ifelse(data$elevation %in% c(600:899),3,
                                     ifelse(data$elevation %in% c(900:1199),4, 
                                            ifelse(data$elevation %in% c(1200:1499),5,
                                                   ifelse(data$elevation>1499,6,NA))))))
# classi di pendenza (come quelle nello script di soil data di Katarina utilizzate per lo Stubai)
data$slope <- round(data$slope)
data$slope_cat <- ifelse(data$slope<=5, 00, ifelse(data$slope %in% c(6:10), 01, ifelse(data$slope %in% c(11:20), 02, ifelse(data$slope %in% c(21:30), 03, ifelse(data$slope %in% c(31:40), 04, ifelse(data$slope %in% c(41:50), 05, ifelse(data$slope %in% c(51:60), 06, ifelse(data$slope %in% c(61:70), 07, ifelse(data$slope %in% c(71:80), 08, ifelse(data$slope %in% c(81:90), 09, ifelse(data$slope %in% c(91:100), 10, ifelse(data$slope %in% c(101:110), 11,12))))))))))))
# classi di esposizione (come quelle nello script di soil data di Katarina utilizzate per lo Stubai)
data$aspect <- round(data$aspect)
data$asp_cat <- ifelse(data$aspect %in% c(0:22), 1, ifelse(data$aspect %in% c(23:68), 2, ifelse(data$aspect %in% c(68:112), 3, ifelse(data$aspect %in% c(113:157), 4, ifelse(data$aspect %in% c(158:202), 5, ifelse(data$aspect %in% c(203:247), 6, ifelse(data$aspect %in% c(248:292),7, ifelse(data$aspect %in% c(293:338),8, ifelse(data$aspect %in% c(339:360),1,0)))))))))          
# aggiungo la colonna Csp_ha al dataframe e riordino le colonne
data <- left_join(data, infc, keep=FALSE)
data <- data %>% select(-1)
data <- data[, c(1, 2, 3, 11, 4, 5, 6, 7, 8, 9, 10, 18, 12, 13, 14, 15, 16, 17)]

#write.csv(data, paste0("C:/Users/seba_/Documents/Seba/Progetti_gitLab/REINFORCE/INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_", Sys.Date(), "_.csv"))


# 3 Campionamento dei valori per gli attributi infc da assegnare alle varie classi ----
data <- read.csv("C:/Users/semarzini/Desktop/Sebastian/Rprojects/REINFORCE/INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_2022-05-10_.csv")
data <- data %>% select(-X)

wgroup <- as.data.frame(table(data$WGRU_CODE))
wgroup$Var1 <- as.character(wgroup$Var1)

ele_data <- list()

# primo ciclo per ogni categoria forestale presente nel dataframe iniziale
for(group in 1:nrow(wgroup)){
  df_wg <- data %>% filter(WGRU_CODE == wgroup$Var1[group])
  # creo un vettore contenente i valori delle varie classi
  classes <- c(0:9)
  # lista vuota da riempire con i valori percentili per ogni classe relativi a tutte le variabili infc
  infc2 <- list()
  # lista vuota per i valori infc delle classi presenti relativi ad una sola variabile
  infc1 <- list()
  # secondo ciclo for in base alle colonne delle variabili di interesse
  for(c in c(5:12)){
    # terzo ciclo for per ogni valore possibile delle classi
    for (n in classes[]){
      # quarto ciclo for per ogni riga del data frame relativo ad ogni categoria forestale
      for(d in 1:nrow(df_wg)){
        if(nrow(df_wg) == 1){
          cont <- count(df_wg, ele_cat == n)[1,2]
          if(cont >= 1 & classes[n] == df_wg$ele_cat[d]){
            infc1[[paste0("class_", n)]][d] <- df_wg[,c][d]
          }
          } else {
            # conto quanti elementi ci sono per ogni classe, se sono più di 1 (compreso) e se 
            # il contatore del ciclo corrisponde al numero della classe creo una lista per ogni variabile
            # infc relativa ad una determinata classe
            cont <- count(df_wg, ele_cat == n)[2,2]
            if(cont >= 1 & classes[n] == df_wg$ele_cat[d]){
            infc1[[paste0("class_", n)]][d] <- df_wg[,c][d]
          }
        }
      }
    }
    # creo una lista di liste. Le sotto liste corrispondono alle variabili infc e presentano i valori
    # per ogni classe altitudinale
    infc2[[colnames(df_wg[c])]] <- infc1
  }
  # per ogni classe altitudinale prendo il 75esimo peercentile
  infc2 <- lapply(infc2, sapply, quantile, prob = (0.75), names = FALSE, na.rm = TRUE)
  # creo una lista finale contenente una lista per ogni categoria forestale con dentro i valori delle variabili infc per ogni classe
  # altitudinale
  ele_data[[wgroup$Var1[group]]] <- infc2
}

# creo un dataframe vuoto da utilizzare successivamente
infc_df <- data.frame(matrix(nrow = 0, ncol = 4))
columns <- c("class_index", "WG", "infc_var", "value")
colnames(empty_df) <- columns

# ciclo per ogni lista relativa ad una categoria forestale
for(e in 1:12){
  # leggo le liste come data frame e trasformo le intestazioni delle righe in colonna
  ds <- as.data.frame(ele_data[e])
  ds <- rownames_to_column(ds, "class_index")
  ds <- pivot_longer(ds, cols = colnames(ds[-1]))
  ds <- separate(data = ds, col = name, into = c("WG", "infc_var"), sep = "\\.")
  # creo un dataframe finale per tutte le categorie forestali con i valori delle variabli infc relativi alle classi altitudinali
  infc_df <- rbind(infc_df, ds)
}




## prova soil sampling 16/05/2022

STarea <- read.csv("C:/Users/semarzini/Desktop/Sebastian/Rprojects/REINFORCE/Dati aree studio/Venosta/soil_venosta_2022-04-12.csv")

for(t in 1:nrow(STarea)){
  STarea$cat_for[t] <- str_extract(STarea$tipi_for[t], "[A-z]+" )
}

STarea$elevation <- round_any(STarea$elevation, 100)
STarea <- STarea %>% select(c(1:15,68))
STarea$xy <- paste(STarea$x, STarea$y, sep="")
STarea$ele_cat <- ifelse(STarea$elevation<300,1, 
                         ifelse(STarea$elevation %in% c(300:599), 2, 
                                ifelse(STarea$elevation %in% c(600:899),3,
                                       ifelse(STarea$elevation %in% c(900:1199),4, 
                                              ifelse(STarea$elevation %in% c(1200:1499),5,
                                                     ifelse(STarea$elevation>1499,6, 0))))))


INFC_Bu <- filter(data, WGRU_CODE == "Bu")
INFC_Ei <- filter(data, WGRU_CODE == "Ei")
INFC_EK <- filter(data, WGRU_CODE == "EK")
INFC_Fi <- filter(data, WGRU_CODE == "Fi")
INFC_Fs <- filter(data, WGRU_CODE == "Fs")
INFC_FT <- filter(data, WGRU_CODE == "FT")
INFC_Ftb <- filter(data, WGRU_CODE == "Ftb")
INFC_Ki <- filter(data, WGRU_CODE == "Ki")
INFC_La <- filter(data, WGRU_CODE == "La")
INFC_Lh <- filter(data, WGRU_CODE == "Lh")
INFC_MH <- filter(data, WGRU_CODE == "MH")
INFC_Zi <- filter(data, WGRU_CODE == "Zi")
#INFC_AE <- filter(data, WGRU_CODE == "AE")
#INFC_AT <- filter(data, WGRU_CODE == "AT")
#INFC_AS <- filter(data, WGRU_CODE == "AS")
#INFC_Ge <- filter(data, WGRU_CODE == "Ge")
#INFC_nf <- filter(data, WGRU_CODE == "nf")
#INFC_Lat <- filter(data, WGRU_CODE == "Lat")


#STarea_filter <- STarea %>% filter(tipi_for=="nf")


###selecting matching sample plots according to aspect, slope and elevation, based on Code by Dominik

select.soil <- function (soil_database_select, elevation, slope, aspect) {
  d <- soil_database_select$idpunto[soil_database_select$elevation==elevation &  soil_database_select$slope==slope & soil_database_select$aspect==aspect]
  if (length(d)==0) {
    print("removing aspect")
    d <- soil_database_select$idpunto[soil_database_select$elevation==elevation & soil_database_select$slope==slope]}
  else{(print(d))}
  
  if (length(d)==0) {
    print("removing slope")
    d <- soil_database_select$idpunto[soil_database_select$elevation==elevation]}
  else{(print(d))}
  
  if (length(d)==0) {
    print("removing elevation")
    d <- soil_database_select$idpunto}
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


for (i in (unique(STarea$cat_for))){
  
  if(exists(paste("INFC_", i, sep="")) == FALSE){
    next
  } else {
    soil_sele <- get(paste("INFC_",i, sep=""))
    point_sele <- STarea[STarea$cat_for==i,]
    
    
    for (q in (c(1:nrow(point_sele)))) {
      aspect <- point_sele[q,"asp_cat"]
      elevation <- point_sele[q,"ele_cat"]
      slope <- point_sele[q,"slope_cat"]
      #xyc <- point_sele[q, "xy"]
      
      g <- select.soil(soil_sele, elevation, slope, aspect)
      
      ID_q <- sampleWithoutSurprises(g)
      
      soil_q <- soil_sele[soil_sele$idpunto==ID_q,]
      
      if(nrow(soil_q) < 1){
        soil_q[+1,] <- NA
        soil_q$WGRU_CODE <- i
        #soil_q$xy <- xyc
        soil_all <- rbind(soil_all, soil_q)
      } else {
        soil_q$WGRU_CODE <- i
        #soil_q$xy <- xyc
        soil_all <- rbind(soil_all, soil_q) 
      }
    }
  }
}

soil_all <- soil_all %>% select(c(4:12, 16:18))
soil_all <- soil_all %>% 
  dplyr::group_by(WGRU_CODE, ele_cat, slope_cat, asp_cat) %>% 
  summarise_all(.funs = mean, na.rm=TRUE)
soil_all <- soil_all %>% rename(cat_for = WGRU_CODE)

infc_raster <- left_join(STarea, soil_all, by = c("cat_for", "ele_cat", "slope_cat", "asp_cat"))

prova <- merge(x=STarea, y=soil_all, by = c("cat_for", "ele_cat"), all.x =TRUE)

soil_raster <- merge(x=STarea, y=soil_all, by = c("cat_for", "ele_cat"), all=TRUE)

for(i in 1:nrow(soil_raster)){
  if(soil_raster[i,]$depth == 0){
    soil_raster[i,][,16:66] <- 0
  }
}

soil_raster <- soil_raster %>% rename(tipi_for = tipi_for.x)

write.csv(soil_raster, paste0("VAL_VENOSTA/soil_venosta_", Sys.Date(), ".csv"), row.names = FALSE )


  