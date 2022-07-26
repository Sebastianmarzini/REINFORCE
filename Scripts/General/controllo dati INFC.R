# 04/05/2022 
# elaborazione e modifica dati INFC relativi a necromassa fine e suolo

library(tidyverse)
library(dplyr)

a <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/puntiAA_infc.csv")
b <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/dati lettiera necromassa fine e suolo_alto adige.csv")

a <- full_join(a,b, by = "idpunto")

c <- a %>% select(idpunto, LAT_WGS84, LON_WGS84, LAT_ND_W84.y, LON_ND_W84.y, COD_FOR, cffddesc1, Wnef_ha, Cnef_ha, Clt_ha, Cor_ha, Css_ha)
c <- c[-which(is.na(c$Wnef_ha)),]
d <- c[-which(is.na(c$LAT_WGS84)),]
e <- c[which(is.na(c$LAT_WGS84)),]

# data frame con coordinate giuste
write.csv(d , "C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti necromassa aa.csv", row.names = FALSE)
# data frame con punti senza coordinate
write.csv(e , "C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/punti necromassa_no coordinate giuste.csv", row.names = FALSE)

# data frame con dati infc e con associate le tipologie forestali dell'alto adige
necromassa <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/infc necromassa e tipi forestali_altoadige.csv")
necromassa <- necromassa %>% filter(WTYP_CODE!="AT")

# data frame con dati infc e coordinate del nodo SW quadrante di riferimento + tipologie forestali dell'alto adige
necromassa2 <- read.csv("C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/infc necromassa e tipi forestali coordinate mancanti_altoadige.csv")

necromassa$tipo <- "coordinate ok"
necromassa2$tipo <- "coordinate non ok"

necromassa <- necromassa %>% select(-COD_FOR, -cffddesc1)
necromassa <- necromassa %>% rename(idpunto = ï..idpunto, lat = LAT_WGS84, lon = LON_WGS84)

necromassa2 <- necromassa2 %>% rename(idpunto = ï..idpunto, lat = LAT_ND_W84, lon = LON_ND_W84)

# data frame finale con dati coordinate giuste e coordinate nodi quadrante + tipologie forestali, escludendo boschi igrofili e ripariali
df_necromassa <- rbind(necromassa, necromassa2)

# csv con tutti i punti disponibili in alto adige per i dati di necromassa fine e suolo
write.csv(df_necromassa , "C:/Users/semarzini/Scientific Network South Tyrol/Mina Marco - Sebastian_Marco_REINFORCE/ITALY_NFI_infc05/Punti_INFC/punti infc alto adige/df_necromassa_altoadige.csv", row.names = FALSE)
