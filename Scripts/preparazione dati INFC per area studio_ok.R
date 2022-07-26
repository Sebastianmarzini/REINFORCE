#05/05/2022
# Elaborazione dati INFC necromassa alto adige da associare all'area di studio in base alla tipologia forestale, l'altitudine,
# la pendenza e l'esposizione

rm(list=ls())

library(sf)
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

# dati INFC necromassa fine e suolo con coordinate giuste
nfs <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/infc necromassa e tipi forestali_altoadige.csv", fileEncoding="UTF-8-BOM")
# tolgo boschi igrofili e colonne che non servono
nfs <- nfs %>% filter(WTYP_CODE!="AT") %>% 
  dplyr::select(-c(Wnef_ha, cffddesc1, COD_FOR))
# dati INFC necromassa popolamento
pn <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/infc popolamento e necromassa alto adige e tipi forestali con coordinate_2022_05_05.csv", fileEncoding="UTF-8-BOM")
# unisco i due data set per idpunto
infc_st <- left_join(pn, nfs, by = "idpunto")
infc_st <- infc_st %>% dplyr::select(-c(WTYP_CODE.y, WTYP_CODE.x))

# creo una nuova colonna per i gruppi forestali: utilizzo il valore del gruppo del secondo dataframe, se è NA uso quello del primo
for(i in 1:nrow(infc_st)){
  infc_st$WGRU_CODE[i] <- ifelse(is.na(infc_st$WGRU_CODE.y[i]) == FALSE, infc_st$WGRU_CODE.y[i], infc_st$WGRU_CODE.x[i])
}

infc_st <- infc_st %>% select(-c(WGRU_CODE.x, WGRU_CODE.y, LAT_WGS84, LON_WGS84))

#write.csv(infc_st, paste0("INFC data South Tyrol/INFC iLand south tyrol_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"), row.names = FALSE)




# 2 Creazione di classi per elevazione, slope e aspect ----

## file creato su gis, associando ai punti infc i dati di elevation, slope e aspect
#data <- read.csv("INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_2022-05-10_.csv")

# classi altitudinali come quelle nel file di deadwood di Katarina per lo Stubai 
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

#write.csv(data, paste0("INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"), row.names = FALSE)




# 3 Campionamento dei valori per gli attributi infc da assegnare alle varie classi ----

data <- read.csv("INFC data South Tyrol/infc SouthTyrol_EleSlopeAspect_2022-05-19_09.21.csv")
data <- data %>% select(-X)

# 3.1 DA IGNORARE: tentativo di campionamento dati infc ma non funziona ####

#wgroup <- as.data.frame(table(data$WGRU_CODE))
#wgroup$Var1 <- as.character(wgroup$Var1)

#ele_data <- list()

# primo ciclo per ogni categoria forestale presente nel dataframe iniziale
# for(group in 1:nrow(wgroup)){
#   df_wg <- data %>% filter(WGRU_CODE == wgroup$Var1[group])
#   # creo un vettore contenente i valori delle varie classi
#   classes <- c(0:9)
#   # lista vuota da riempire con i valori percentili per ogni classe relativi a tutte le variabili infc
#   infc2 <- list()
#   # lista vuota per i valori infc delle classi presenti relativi ad una sola variabile
#   infc1 <- list()
#   # secondo ciclo for in base alle colonne delle variabili di interesse
#   for(c in c(5:12)){
#     # terzo ciclo for per ogni valore possibile delle classi
#     for (n in classes[]){
#       # quarto ciclo for per ogni riga del data frame relativo ad ogni categoria forestale
#       for(d in 1:nrow(df_wg)){
#         if(nrow(df_wg) == 1){
#           cont <- count(df_wg, ele_cat == n)[1,2]
#           if(cont >= 1 & classes[n] == df_wg$ele_cat[d]){
#             infc1[[paste0("class_", n)]][d] <- df_wg[,c][d]
#           }
#           } else {
#             # conto quanti elementi ci sono per ogni classe, se sono piu' di 1 (compreso) e se
#             # il contatore del ciclo corrisponde al numero della classe creo una lista per ogni variabile
#             # infc relativa ad una determinata classe
#             cont <- count(df_wg, ele_cat == n)[2,2]
#             if(cont >= 1 & classes[n] == df_wg$ele_cat[d]){
#             infc1[[paste0("class_", n)]][d] <- df_wg[,c][d]
#             }
#           }
#       }
#     }
# 
#     # creo una lista di liste. Le sotto liste corrispondono alle variabili infc e presentano i valori
#     # per ogni classe altitudinale
#     infc2[[colnames(df_wg[c])]] <- infc1
#   }
#   # per ogni classe altitudinale prendo il 75esimo peercentile
#   infc2 <- lapply(infc2, sapply, quantile, prob = (0.75), names = FALSE, na.rm = TRUE)
#   # creo una lista finale contenente una lista per ogni categoria forestale con dentro i valori delle variabili infc per ogni classe
#   # altitudinale
#   ele_data[[wgroup$Var1[group]]] <- infc2
# }
# 
# # creo un dataframe vuoto da utilizzare successivamente
# # infc_df <- data.frame(matrix(nrow = 0, ncol = 4))
# # columns <- c("class_index", "WG", "infc_var", "value")
# # colnames(empty_df) <- columns
# 
# # ciclo per ogni lista relativa ad una categoria forestale
# for(e in 1:12){
#   # leggo le liste come data frame e trasformo le intestazioni delle righe in colonna
#   ds <- as.data.frame(ele_data[e])
#   ds <- rownames_to_column(ds, "class_index")
#   ds <- pivot_longer(ds, cols = colnames(ds[-1]))
#   ds <- separate(data = ds, col = name, into = c("WG", "infc_var"), sep = "\\.")
#   # creo un dataframe finale per tutte le categorie forestali con i valori delle variabli infc relativi alle classi altitudinali
#   infc_df <- rbind(infc_df, ds)
# } 


# 3.2 SOIL SAMPLING: usato codice di Katharina e risistemato ####

# carico il raster stack dell'area di studio
STarea <- read.csv("Dati aree studio/Venosta/soil_venosta_2022-04-12.csv")
# estraggo categorie forestali dai tipi
for(t in 1:nrow(STarea)){
  STarea$cat_for[t] <- str_extract(STarea$tipi_for[t], "[A-z]+" )
}
# creo classi altitduinali da script di Katharina
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

# seleziono colonne categorie forestali, dati infc e classi di altitudine, pendenza ed esposizione
soil_all <- soil_all %>% select(c(2:10, 14:16))

soil_esa <- soil_all %>% 
  dplyr::group_by(WGRU_CODE, ele_cat, slope_cat, asp_cat) %>% 
  summarise_all(.funs = mean, na.rm=TRUE)
soil_esa <- soil_esa %>% rename(cat_for = WGRU_CODE)

 soil_es <- soil_all %>% 
  dplyr::group_by(WGRU_CODE, ele_cat, slope_cat) %>% 
  summarise_all(.funs = mean, na.rm=TRUE) %>% 
  select(-asp_cat)
soil_es <- soil_es %>% rename(cat_for = WGRU_CODE)

soil_e <- soil_all %>% 
  dplyr::group_by(WGRU_CODE, ele_cat) %>% 
  summarise_all(.funs = mean, na.rm=TRUE) %>% 
  select(-c(slope_cat, asp_cat))
soil_e <- soil_e %>% rename(cat_for = WGRU_CODE)

# left join tra dati area studio e dati infc mediati per categoria forestale, altitudine, pendenza ed esposizione
infc_raster_esa <- left_join(STarea, soil_esa, by = c("cat_for", "ele_cat", "slope_cat", "asp_cat"))

# estrazione dei dati non NA
nonNa_esa <- infc_raster_esa[!is.na(infc_raster_esa$Capm_ha) & !is.na(infc_raster_esa$Cce_ha) & !is.na(infc_raster_esa$Cne_ha),]
  
# left join tra il database ottenuto dal join precedente (solo NA!!) e quello con i valori infc calcolati per categoria forestale,
# altitudine e pendenza
join1 <- left_join(infc_raster_esa[is.na(infc_raster_esa$Capm_ha) & is.na(infc_raster_esa$Cce_ha) & is.na(infc_raster_esa$Cne_ha) & 
                                     is.na(infc_raster_esa$Cnef_ha) & is.na(infc_raster_esa$Clt_ha) & is.na(infc_raster_esa$Cor_ha) &
                                     is.na(infc_raster_esa$Css_ha) & is.na(infc_raster_esa$Csp_ha),],
                   soil_es, by = c("cat_for", "ele_cat", "slope_cat"), keep = FALSE)
join1 <- select(join1, -c(17:24))
colnames(join1)[17:24] <- sub("*\\.[A-z]", "", colnames(join1)[17:24])

nonNa_es <- join1[!is.na(join1$Capm_ha) & !is.na(join1$Cce_ha) & !is.na(join1$Cne_ha),]

# stesso procedimento di sopra ma i valori infc sono calcolati per categoria forestale e altitudine
join2 <- left_join(join1[is.na(join1$Capm_ha) & is.na(join1$Cce_ha) & is.na(join1$Cne_ha) & is.na(join1$Cnef_ha) & 
                           is.na(join1$Clt_ha) & is.na(join1$Cor_ha) & is.na(join1$Css_ha) & is.na(join1$Csp_ha),], 
                   soil_e, by = c("cat_for", "ele_cat"), keep = FALSE)

join2 <- select(join2, -c(17:24))
colnames(join2)[17:24] <- sub("*\\.[A-z]", "", colnames(join2)[17:24])

# unisco insieme: i dati infc del primo join, i dati infc del secondo join e quelli del terzo join
ru_infc <- rbind(nonNa_esa, nonNa_es, join2)

# per i dati di necromassa fine e suolo ci sono pochi dati quindi uso quelli disponibili e li estendo a tutta la categoria forestale
dwd <- ru_infc %>% select(cat_for, Cnef_ha, Clt_ha, Cor_ha, Css_ha, Csp_ha)
dwd_cf <- dwd %>% 
  dplyr::group_by(cat_for) %>% 
  summarise_all(.funs = mean, na.rm=TRUE) %>% 
  na.omit()
dwd_cf <- na.omit(dwd_cf)

for(i in 1:nrow(dwd_cf)){
  for(l in 1:nrow(ru_infc)){
    if(dwd_cf$cat_for[i] == ru_infc$cat_for[l]){
      ru_infc$Cnef_ha[l] <- dwd_cf$Cnef_ha[i]
      ru_infc$Clt_ha[l] <- dwd_cf$Clt_ha[i]
      ru_infc$Cor_ha[l] <- dwd_cf$Cor_ha[i]
      ru_infc$Css_ha[l] <- dwd_cf$Css_ha[i]
      ru_infc$Csp_ha[l] <- dwd_cf$Csp_ha[i]
    }
  }
}

# la categoria forestale del lariceto (La) non ha dati INFC per la classe altitudinale 5 quindi faccio una media tra la 4 e la 6
for(i in 1:nrow(ru_infc)){
  if(ru_infc$cat_for[i] == "La" & ru_infc$ele_cat[i] == 5){
    ru_infc$Capm_ha[i] <- mean(subset(ru_infc$Capm_ha, ru_infc$ele_cat == 6 & ru_infc$cat_for == "La"))/2
    ru_infc$Cce_ha[i] <- mean(subset(ru_infc$Cce_ha, ru_infc$ele_cat == 6 & ru_infc$cat_for == "La"))/2
    ru_infc$Cne_ha[i] <- mean(subset(ru_infc$Cne_ha, ru_infc$ele_cat == 6 & ru_infc$cat_for == "La"))/2
  } 
}

# 1 - non ci sono dati di necromassa fine e suolo per Fi e EK:
#     per Fi uso quelli di Fs, per EK faccio media tra Ei e Ki;
# 2 - metto a 0 i vari valori di carbonio per le resource units con profondità == 0
for(i in 1:nrow(ru_infc)){
  if(ru_infc$cat_for[i] == "Fi") {
    ru_infc$Cnef_ha[i] <- ru_infc$Cnef_ha[ru_infc$cat_for == "Fs"][1]
    ru_infc$Clt_ha[i] <- ru_infc$Clt_ha[ru_infc$cat_for == "Fs"][1]
    ru_infc$Cor_ha[i] <- ru_infc$Cor_ha[ru_infc$cat_for == "Fs"][1]
    ru_infc$Css_ha[i] <- ru_infc$Css_ha[ru_infc$cat_for == "Fs"][1]
    ru_infc$Csp_ha[i] <- ru_infc$Csp_ha[ru_infc$cat_for == "Fs"][1]
  }
  if(ru_infc$cat_for[i] == "EK") {
    ru_infc$Cnef_ha[i] <- (ru_infc$Cnef_ha[ru_infc$cat_for == "Ei"][1] + ru_infc$Cnef_ha[ru_infc$cat_for == "Ki"][1])/2
    ru_infc$Clt_ha[i] <- (ru_infc$Clt_ha[ru_infc$cat_for == "Ei"][1] + ru_infc$Clt_ha[ru_infc$cat_for == "Ki"][1])/2
    ru_infc$Cor_ha[i] <- (ru_infc$Cor_ha[ru_infc$cat_for == "Ei"][1] + ru_infc$Cor_ha[ru_infc$cat_for == "Ki"][1])/2
    ru_infc$Css_ha[i] <- (ru_infc$Css_ha[ru_infc$cat_for == "Ei"][1] + ru_infc$Css_ha[ru_infc$cat_for == "Ki"][1])/2
    ru_infc$Csp_ha[i] <- (ru_infc$Csp_ha[ru_infc$cat_for == "Ei"][1] + ru_infc$Csp_ha[ru_infc$cat_for == "Ki"][1])/2
  }
  if(ru_infc$depth[i] == 0){
    ru_infc$Capm_ha[i] <- 0
    ru_infc$Cce_ha[i] <-  0
    ru_infc$Cne_ha[i] <-  0
    ru_infc$Cnef_ha[i] <- 0
    ru_infc$Clt_ha[i] <- 0
    ru_infc$Cor_ha[i] <- 0
    ru_infc$Css_ha[i] <- 0
    ru_infc$Csp_ha[i] <- 0
  }
}

# creo colonna SOMC per dati fuori aree forestali e ci inserisco dei valori medi calcolati con quelli del BMS
ru_infc$SOMC_nf <- NA
for(i in 1:nrow(ru_infc)){
  ifelse(startsWith(ru_infc$Bodenlands[i], "EW") == TRUE, ru_infc$SOMC_nf[i] <- 176215,
         ifelse(ru_infc[i,]$Bodenlands == "AGh_EW-IA-MA_o2200", ru_infc$SOMC_nf[i] <- 260680,
                ifelse(ru_infc[i,]$Bodenlands == "AGh_EW-UC-UG_o2200", ru_infc$SOMC_nf[i] <- 260680,
                       ifelse(ru_infc[i,]$Bodenlands == "AGh-MB_IU", ru_infc$SOMC_nf[i] <- 260680,
                              ifelse(ru_infc[i,]$Bodenlands == "AGh-UF", ru_infc$SOMC_nf[i] <- 260680,
                                     ifelse(ru_infc[i,]$Bodenlands == "AGh-UG-GB", ru_infc$SOMC_nf[i] <- 260680,
                                            ifelse(ru_infc[i,]$Bodenlands == "AGh-U-Undiff", ru_infc$SOMC_nf[i] <- 260680,
                                                   ifelse(ru_infc[i,]$Bodenlands == "AGh_EW-IA-MA_u2200", ru_infc$SOMC_nf[i] <- 229213.2143,
                                                          ifelse(ru_infc[i,]$Bodenlands == "AGh_EW-UC-UG_u2200", ru_infc$SOMC_nf[i] <- 229213.2143,
                                                                 ifelse(startsWith(ru_infc$Bodenlands[i], "ZSG") == TRUE, ru_infc$SOMC_nf[i] <- NA,
                                                                        ifelse(ru_infc[i,]$Bodenlands == "TR", ru_infc$SOMC_nf[i] <- 107663.1111,
                                                                               ifelse(ru_infc[i,]$Bodenlands == "Fels" & ru_infc[i,]$depth > 0 & ru_infc[i,]$tipi_for == "nf", ru_infc$SOMC_nf[i] <- 24960, NA))))))))))))
}

write.csv(ru_infc, paste0("Dati aree studio/Venosta/infcVenosta_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"), row.names = FALSE)

#write.csv(infcv, paste0("Dati aree studio/Venosta/infcVenosta_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"), row.names = FALSE)

navtf <- read.csv("Dati aree studio/Venosta/Nav_tipiForestali.csv") %>% rename (tipi_for = Forest.Type)

infcv <- read.csv("Dati aree studio/Venosta/infcVenosta_2022-07-15_16.41.csv")

# inserisco i valori di Nav calcolati per le varie tipologie forestali

infcv <- left_join(infcv, navtf, by = "tipi_for")

# utilizzo valori di necromassa fine e suolo del larice-cembreta per il lariceto

for(i in 1:nrow(infcv)){
  if(infcv$cat_for[i] == "La"){
    infcv$Cnef_ha[i] <- infcv$Cnef_ha[infcv$cat_for == "Zi"][1]
    infcv$Clt_ha[i] <- infcv$Clt_ha[infcv$cat_for == "Zi"][1]
    infcv$Cor_ha[i] <- infcv$Cor_ha[infcv$cat_for == "Zi"][1]
    infcv$Css_ha[i] <- infcv$Css_ha[infcv$cat_for == "Zi"][1]
    infcv$Csp_ha[i] <- infcv$Csp_ha[infcv$cat_for == "Zi"][1]
  }
  if(infcv$cat_for[i] == "EK" & infcv$ele_cat[i] == 5){
    infcv$Capm_ha[i] <- mean(infcv$Capm_ha[infcv$cat_for == "EK" & infcv$ele_cat == 4])
    infcv$Cce_ha[i] <- mean(infcv$Cce_ha[infcv$cat_for == "EK" & infcv$ele_cat == 4])
    infcv$Cne_ha[i] <- mean(infcv$Cne_ha[infcv$cat_for == "EK" & infcv$ele_cat == 4])
  } 
  # Lh è presente solo in due resource units.Do 0 a i valori di necromassa fine e suolo
  if(infcv$cat_for[i] == "Lh"){
    infcv$Cnef_ha[i] <- 0
    infcv$Clt_ha[i] <- 0
    infcv$Cor_ha[i] <- 0
    infcv$Css_ha[i] <- 0
    infcv$Csp_ha[i] <- 0
  }
}

# inserisco valore SOMC per bodenlands MW che avevo dimenticato precedentemente. Il valore è calcolato mediando quelli delle altre
# categorie relative a pascoli o praterie da sfalcio

for(i in 1:nrow(infcv)){
  if(infcv[i,]$Bodenlands == "MW"){
    infcv$SOMC_nf[i] <- 186886.5302
  }
}

# converto i dati INFC in kg/ha
infcv$Capm_ha <- infcv$Capm_ha*1000
infcv$Cce_ha <- infcv$Cce_ha*1000
infcv$Cne_ha <- infcv$Cne_ha*1000
infcv$Cnef_ha <- infcv$Cnef_ha*1000
infcv$Clt_ha <- infcv$Clt_ha*1000
infcv$Cor_ha <- infcv$Cor_ha*1000
infcv$Css_ha <- infcv$Css_ha*1000
infcv$Csp_ha <- infcv$Csp_ha*1000

#infcv <- read.csv("Dati aree studio/Venosta/infcVenosta_2022-06-28_15.28_old.csv")

# creo le colonne per le variabili del project file di iLand
infcv$youngLabileC <-  infcv$Cnef_ha + infcv$Clt_ha + infcv$Cor_ha
infcv$somC = infcv$Css_ha + infcv$Csp_ha

infcv <- infcv %>% rename(swdC = Capm_ha,
                          otherC = Cce_ha,
                          youngRefractoryC = Cne_ha)

infcv <- infcv %>% select(-c(Cnef_ha, Clt_ha, Cor_ha, Css_ha, Csp_ha))

# per le categorie senza dati uso quelli di Katharina da deadwood_tyrol in base alle classi altitudinali
for(i in 1:nrow(infcv)){
  if(infcv$cat_for[i] == "Ge" | infcv$cat_for[i] == "Lat" | infcv$cat_for[i] == "AT" | infcv$cat_for[i] == "AS" | infcv$cat_for[i] == "AE" & infcv$ele_cat[i] == 3){
    infcv$swdC[i] <- 4.4*250
    infcv$somC[i] <- 0
    infcv$youngLabileC[i] <- 0
    infcv$youngRefractoryC[i] <- 0
    infcv$otherC[i] <- 4.4*250*1.62
  }
  if(infcv$cat_for[i] == "Ge" | infcv$cat_for[i] == "Lat" | infcv$cat_for[i] == "AT" | infcv$cat_for[i] == "AS" | infcv$cat_for[i] == "AE" & infcv$ele_cat[i] == 4){
    infcv$swdC[i] <- 7.7*250
    infcv$somC[i] <- 0
    infcv$youngLabileC[i] <- 0
    infcv$youngRefractoryC[i] <- 0
    infcv$otherC[i] <- 7.7*250*1.62
  }
  if(infcv$cat_for[i] == "Ge" | infcv$cat_for[i] == "Lat" | infcv$cat_for[i] == "AT" | infcv$cat_for[i] == "AS" | infcv$cat_for[i] == "AE" & infcv$ele_cat[i] == 5){
    infcv$swdC[i] <- 8.2*250
    infcv$somC[i] <- 0
    infcv$youngLabileC[i] <- 0
    infcv$youngRefractoryC[i] <- 0
    infcv$otherC[i] <- 8.2*250*1.62
  }
  if(infcv$cat_for[i] == "Ge" | infcv$cat_for[i] == "Lat" | infcv$cat_for[i] == "AT" | infcv$cat_for[i] == "AS" | infcv$cat_for[i] == "AE" & infcv$ele_cat[i] == 6){
    infcv$swdC[i] <- 10.2*250
    infcv$somC[i] <- 0
    infcv$youngLabileC[i] <- 0
    infcv$youngRefractoryC[i] <- 0
    infcv$otherC[i] <- 8.2*250*1.62
  }
}


write.csv(infcv, paste0("Dati aree studio/Venosta/infcVenosta_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"), row.names = FALSE)


shp <- st_read("C:/Users/seba_/One Drive Eurac/OneDrive - Scientific Network South Tyrol/Shared/Sebastian_Marco_REINFORCE/VAL_VENOSTA/GIS/ProjectArea.shp")
ds <- read.csv("C:/Users/semarzini/OneDrive - Scientific Network South Tyrol/Sebastian/Rprojects/REINFORCE/Dati aree studio/Venosta/infcVenosta_2022-07-15_16.41.csv")


# 4 Plot dei dati --------

# plot(shp$geometry)
# 
# pdf("swdC.pdf", width = 9, height = 14)
# plot(
#   rasterFromXYZ(ds[,c(2,3,17)]),
#   main = "swdC [kg/ha]",
#   col = rainbow(5))
# plot(shp$geometry, add = TRUE)
# dev.off()
# 
# summary(ds$somC)
# 
# hist(ds$youngLabileC, col = rainbow(1), border = "black", main = "youngLabileC values distribution", 
#      xlab = "youngLabileC [kg/ha]")
# 
# ggplot(ds, aes(x=somC)) + 
#   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
#                  binwidth=2,
#                  colour="black", fill="green") +
#   geom_density(alpha=.3, fill="#FF6666")

# 5 Creazione environment file ----

cc <- load("Dati aree studio/Venosta/cluster800_object_map.RData") ## IMPORTANT!!! generated in script climate_clustering.R

iland.ru.id.df <- data.frame(rasterToPoints(iland.ru.id, xy=TRUE)) 
cluster.map.df <- data.frame(rasterToPoints(cluster.map, xy=TRUE)) 

ruClimate_join <- left_join(iland.ru.id.df, cluster.map.df, by = c("x", "y"))
ruClimate_join$cell <- paste0("climate_", ruClimate_join$cell)

ruClimate_join$xy <- paste(ruClimate_join$x, ruClimate_join$y, sep="")
env <-  merge(x=ruClimate_join, y=ds, by="xy", all=TRUE)

env <- env %>% select(-c(x.x, y.x))
env <- env %>% rename(r_unit = layer, x = x.y, y = y.y)

env <- env %>% select(-c(xy, x, y, elevation, slope, aspect, tipi_for, Bodenlands, ele_cat, slope_cat, asp_cat, cat_for))

env <- env %>% mutate(somC = ifelse(somC %in% NA, SOMC_nf, somC))

env <- env %>% select(-SOMC_nf) %>% rename(id = r_unit, model.climate.tableName = cell, 
                                           model.site.availableNitrogen = Nav,
                                           model.site.soilDepth = depth,
                                           model.site.pctSand = sand,
                                           model.site.pctSilt = silt,
                                           model.site.pctClay = clay,
                                           model.site.youngLabileC = youngLabileC,
                                           model.site.somC = somC,
                                           model.site.youngRefractoryC = youngRefractoryC,
                                           model.site.otherC = otherC,
                                           model.site.swdC = swdC)

env$model.climate.tableName <- sub("_", "", env$model.climate.tableName)


write.csv(env, paste0("C:/Users/semarzini/OneDrive - Scientific Network South Tyrol/Sebastian/Rprojects/REINFORCE/Dati aree studio/Venosta/environment_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".csv"), row.names = FALSE) 




