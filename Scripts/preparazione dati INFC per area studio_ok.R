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

# 1 Sistemazione dati INFC ----

# csv con dati relativi all'area di studio
soil_raster <- read.csv("C:/Users/seba_/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/VAL_VENOSTA/soil_venosta_2022-04-12.csv")

# dati INFC necromassa fine e suolo con coordinate giuste
nfs <- read.csv("C:/Users/seba_/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/infc necromassa e tipi forestali_altoadige.csv", fileEncoding="UTF-8-BOM")

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

#write.csv(infc, paste0("C:/Users/semarzini/Documents/REINFORCE/iLand/INFC data South Tyrol/dati utili INFC alto adige_", Sys.Date(), ".csv"), row.names = FALSE)

# 2 Creazione di classi per elevazione, slope e aspect ----

#data <- read.csv("C:/Users/seba_/Documents/Seba/Progetti_gitLab/REINFORCE/INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_20220509_1005.csv")

# classi altitudinali come quelle nel file di deadwood di Katarina per lo Stubai pi? altre classi sopra i 1500 m: 
data$elevation <- round(data$elevation)
data$ele_cat <- ifelse(data$elevation<=300, 1, ifelse(data$elevation %in% c(300:599), 2, ifelse(data$elevation %in% c(600:899), 3, ifelse(data$elevation %in% c(900:1199), 4, ifelse(data$elevation %in% c(1200:1499), 5, ifelse(data$elevation %in% c(1500:1799), 6, ifelse(data$elevation %in% c(1800:1999), 7, ifelse(data$elevation > 1999, 8, 9))))))))
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

write.csv(data, paste0("C:/Users/seba_/Documents/Seba/Progetti_gitLab/REINFORCE/INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_", Sys.Date(), "_.csv"))

# 3 Campionamento dei valori per gli attributi infc da assegnare alle varie classi ----
data <- read.csv("C:/Users/seba_/Documents/Seba/Progetti_gitLab/REINFORCE/INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_2022-05-10_.csv")

wgroup <- as.data.frame(table(data$WGRU_CODE))
wgroup$Var1 <- as.character(wgroup$Var1)
ele_data <- list()
# primo ciclo per ogni categoria forestale presente nel dataframe iniziale
for(group in 1:nrow(wgroup)){
  df <- data %>% filter(WGRU_CODE == wgroup$Var1[group])
  # creo un vettore contenente i valori delle varie classi
  classes <- c(1:9)
  # lista vuota da riempire con i valori mediani per ogni classe relativi a tutte le variabili infc
  sample <- list()
  # lista vuota per i valori infc delle classi presenti relativi ad una sola variabile
  values <- list()
  # secondo ciclo for in base alle colonne delle variabili di interesse
  for(c in c(5:12)){
    # terzo ciclo for per ogni valore possibile delle classi
    for (n in classes[]){
      # quarto ciclo for per ogni riga del data frame relativo ad ogni categoria forestale
      for(d in 1:nrow(df)){
        if(nrow(df) == 1){
          cont <- count(df, ele_cat == n)[1,2]
          if(cont >= 1 & classes[n] == df$ele_cat[d]){
            values[[paste0("class_", n)]][d] <- df[,c][d]
          }
          } else {
            # conto quanti elementi ci sono per ogni classe, se sono più di 1 (compreso) e se 
            # il contatore del ciclo corrisponde al numero della classe creo una lista per ogni variabile
            # infc relativa ad una determinata classe
            cont <- count(df, ele_cat == n)[2,2]
            if(cont >= 1 & classes[n] == df$ele_cat[d]){
            values[[paste0("class_", n)]][d] <- df[,c][d]
          }
        }
      }
    }
    # creo una lista di liste. Le sotto liste corrispondono alle variabili infc e presentano i valori
    # per ogni classe altitudinale
    sample[[colnames(df[c])]] <- values
  }
  # calcolo il valore medio per ogni classe
  sample <- lapply(sample, sapply, median, na.rm = TRUE)
  ele_data[[wgroup$Var1[group]]] <- sample
}






  