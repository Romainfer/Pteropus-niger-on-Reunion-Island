###################################################################################################################################################
#                                                                                                                                                 #
#                                 Etude de l'ecologie de la Roussette Noire (Pteropus niger) sur l Ile de La Reunion                              #
#                                                                                                                                                 #
#                         -- Script de transformation des donnees GPS et de terrain pour faire la base de donnees finale --                       #
#                                                                                                                                                 #               
#                                                         ------ SCRIPT 3.1 ------                                                                #
#                                                                                                                                                 #
#                                                        Romain FERNANDEZ - Mai 2020                                                              #
#                                                                                                                                                 #
###################################################################################################################################################
installed.packages("sf")
installed.packages("sp")
installed.packages("raster")
install.packages("rgdal")
install.packages("ICSNP")
install.packages("plotKML")
install.packages("lubridate")
install.package("dplyr")


install.packages("tibble")
#install.packages("stringr")

library(sf)
library(sp)
library(raster)
library(rgdal)
library(ICSNP) # point median
library(plotKML) # import GPX
library(lubridate) # work with time
library(dplyr)



################## Chemins

BDD <- paste("D:/Donnees Terrain/Dalles/Dalle_1329 IDok", "Fiche_terrain_1329", sep="/") #Feuille de terrain

Garmin_1 <- paste("D:/Donnees Terrain/Dalles/Dalle_1329 IDok", "Piste_2020-06-12 133450_GPS_neuf_1329", sep = "/") #Tracer GPX GPS1

Garmin_2 <- paste("D:/Donnees Terrain/Dalles/Dalle_1329 IDok", "Piste_2020-06-12 133450_GPS_pas_neuf_1329", sep = "/") #Tracer GPX GPS2

#MILSAR <- paste("D:/SIG PTNIG/2_Resultats/Dalles_terrain/Test_BDD/", "987009", sep = "")

sortie <- paste("D:/Donnees Terrain/Dalles/Dalle_1329 IDok", "BDD_finale_dalle_1329", sep ="/")


  
################## STEP 1 : import BDD et GPX  -------

### Import BDD terrain
BDD_terrain <- read.csv2(paste(BDD, ".csv", sep =""))


### Import fichier GPX Garmin 1 et garmin 2
GPX1 <- readGPX(paste(Garmin_1, ".gpx", sep = ""))
GPX1_df <- data.frame(GPX1$tracks[[1]])
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time" )

GPX2 <- readGPX(paste(Garmin_2, ".gpx", sep = ""))
GPX2_df <- data.frame(GPX2$tracks[[1]])
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time" )

GPX_bind <- rbind(GPX1_df, GPX2_df)



# ### Import des points de la balise MILSAR
# BDD_MILSAR <- read.csv2(paste(MILSAR, ".csv", sep = ""))
# 
# extract_MILSAR <- BDD_MILSAR[ , c("Latitude.decimal", "Longitude.decimal", "Altitude", "Timestamp")]
# 
# # Transformer les coordonnees en variable numerique
# extract_MILSAR$Longitude.decimal <- as.numeric((as.character(extract_MILSAR$Longitude.decimal)))
# extract_MILSAR$Latitude.decimal <- as.numeric((as.character(extract_MILSAR$Latitude.decimal)))
# 
# colnames(extract_MILSAR) <- c("Lat", "Lon", "ele", "time")
# 
# GPX_bind <- rbind(GPX_bind, extract_MILSAR)
# 


################## STEP 1 BIS : Import des donnees GPS Garmin de fichiers shapefile et pas en GPX -----
# INFORMATIONS : Partie utilise lorsque les traces GPS doivent etre modifie et que le nouvel encodage
# est en shapefile, car encodage en GPX difficile ou fois les donnees GPX modifiees.


# ## Import BDD terrain
# BDD_terrain <- read.csv2(paste(BDD, ".csv", sep =""))
# 
# 
# GPX1 <-read_sf(paste(Garmin_1, ".shp", sep = ""))
# coord <- st_coordinates(GPX1)
# GPX1 <- cbind(GPX1, coord) # Bien assemble dans le bonne ordre, verification ok en comparant colonne Z et ele
# GPX1 <- as_Spatial(GPX1)
# GPX1 <- data.frame(GPX1)
# GPX1 <- subset(GPX1, select = c(X, Y, ele, time))
# colnames(GPX1) <- c("Lon", "Lat", "ele", "time" )
# 
# 
# GPX2 <-read_sf(paste(Garmin_2, ".shp", sep = ""))
# coord <- st_coordinates(GPX2)
# GPX2 <- cbind(GPX2, coord) # Bien assemble dans le bonne ordre, verification ok en comparant colonne Z et ele
# GPX2 <- as_Spatial(GPX2)
# GPX2 <- data.frame(GPX2)
# GPX2 <- subset(GPX2, select = c(X, Y, ele, time))
# colnames(GPX2) <- c("Lon", "Lat", "ele", "time" )
# 
# GPX_bind <- rbind(GPX1, GPX2)



################## STEP 2 : Mise en forme des donnees -------


### Mise en forme des colonnes temps dans la BDD terrain et le fichier GPX
GPX_bind <- mutate(GPX_bind, time = ymd_hms(time)) #Mise en forme de la colonne time dans GPX

BDD_terrain_time <- dmy_hm(BDD_terrain$Heure) # Extraction vecteur et mise en forme des heure du fichier terrain


### Prendre en compte le decallage des fuseaux horaires entre les fichiers GPX des GPS
decalage <- period(hour = 4)
BDD_terrain_time <- BDD_terrain_time - decalage


### Creation d un vecteur temps issu de la colonne "heure" de la BDD terrain et y ajouter +10 min
per <- period(minutes = 10)
BDD_terrain_time_10 <- BDD_terrain_time + per





################## STEP 3 : Calcule des points medians en fonction des heures de saisies -------

### Filtrer tous les points situes entre les deux vecteurs crees precedement 

pt_periode <- data.frame(1,4)

BDD_GPX_median <- data.frame(1,2)
colnames(BDD_GPX_median) <- c("Lon", "Lat")

for (i in 1:length(BDD_terrain_time)) {
  
  print(i)
  pt_periode <- filter(GPX_bind, GPX_bind$time >= BDD_terrain_time[i] & GPX_bind$time < BDD_terrain_time_10[i])
  pt_coord <- pt_periode[ ,c("Lon", "Lat")]
  pt_median <- try(spatial.median(pt_coord, maxiter = 1000))
  
  if(!inherits(pt_median, "try-error")){
    df_median <- t(data.frame(pt_median))
    BDD_GPX_median <- rbind(BDD_GPX_median, df_median)
    
  } else{
    vect_nul <- c("NA", "NA")
    BDD_GPX_median <- rbind(BDD_GPX_median, vect_nul)
  }
  
}




### Enlever la premiere ligne du dataframe correspondant a la ligne de creation 
BDD_GPX_median <- BDD_GPX_median[-1,]

### Ajouter au tableau des points median les heures de debut en ajoutant le decalage UTM enleve precedement
decalage <- period(hour = 4)
BDD_terrain_time <- BDD_terrain_time + decalage

BDD_GPX_median <- data.frame(BDD_GPX_median, BDD_terrain_time) 

colnames(BDD_GPX_median) <- c("Longitude", "Latitude", "Heure") #Renomer la colonne "time" en "heure" pour la jointure step 4

#write.csv(BDD_GPX_median, file = paste(paste(sortie), "_pt_median_GPX", ".csv", sep = ""))


################## STEP 4 : Jointure de la BDD GPX point median nouvellement cree et BDD terrain -------

### Preparer la jointure : Suprimer la colonne "heure" et en ajouter une de type object posixct pour realiser la jointure par les heures
Heure <- dmy_hm(BDD_terrain$Heure)
BDD_terrain <- data.frame(BDD_terrain, Heure)

BDD_terrain <- BDD_terrain[ , -3]
colnames(BDD_terrain) <- c("Cas", "Type", "Espece latin", "Espece vernaculaire", "Numero Photo", "Stade", "Accessibilite du polygone", "Observateur", "Heure")


### Preparer la jointure : Suprimer les lignes contenant aucune coordonnees de point median pour ne pas influencer la jointure
BDD_GPX_median <- na.omit(BDD_GPX_median)


### Jointure
BDD_tot <- left_join(BDD_terrain, BDD_GPX_median, by = "Heure")

write.csv(BDD_tot, file = paste(sortie, ".csv", sep = ""))



