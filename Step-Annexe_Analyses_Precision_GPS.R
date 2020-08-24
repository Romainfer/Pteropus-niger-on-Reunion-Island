###################################################################################################################################################
#                                                                                                                                                 #
#                                 Etude de l'ecologie de la Roussette Noire (Pteropus niger) sur l Ile de La Reunion                              #
#                                                                                                                                                 #
#                               Script d analyse de la precicion des points GPS des arbres en deux parties distinctes : 
#                                      par modalites (partie I) et sur une un labse de temps plus long (partie II)                                                #
#                                                                                                                                                 #               
#                                                         ------ SCRIPT PARTIE annexe------                                                       #
#                                                                                                                                                 #
#                                                           Romain FERNANDEZ - Juin 2020                                                          #
#                                                                                                                                                 #
###################################################################################################################################################


installed.packages("sf")
installed.packages("sp")
installed.packages("raster")
install.packages("rgdal")
install.packages("ICSNP")
install.packages("plotKML")
install.packages("dplyr")
install.packages("geosphere")
install.packages("ggplot2")

#install.packages("stringr")

library(sf)
library(sp)
library(raster)
library(rgdal)
library(ICSNP) # point median
library(plotKML) # import GPX
library(dplyr)
library(geosphere) #calcule distance
library(ggplot2)
library(reshape2) #fonction melt

#library(stringr) # selection caractere





############ PREMIERE PARTIE : Traitements des donnees pour les balises GPS MILSAR ---------

############ STEP 1 : Modifier les chemins ci-dessous avant de lancer le scrip

chemin_BDD <- "H:/Precisions_GPS/Test_4_Balise_MILSAR/987010" # Entrer le chemin du fichier tableur (avec .csv) contenant les points GPS

Latitude <- "Latitude.decimal"   # Entrer le nom de la colonne contenant les donnÃ©es Latitude dans le fichier shapefile precedent (donnÃ©e projetees = nombre numerique)
Longitude <- "Longitude.decimal"  # Entrer le nom de la colonne contenant les donnÃ©es Longitude dans le fichier shapefile precedent (donnÃ©e projetees = nombre numerique
ID_point <- "name"         # Entrer le nom de la colonne stockant l identifiant du point dans le fichier shapefile precedent


############ STEP 2 : lancer le script

# Entrer la base de donnee
BDD <- read.csv2(paste(chemin_BDD, ".csv", sep = ""))


# Extraire colonne des coordonnees GPS (Latitude et Longitude)
extract <- BDD[ , c(Longitude, Latitude)]

extract$Longitude.decimal <- as.numeric((as.character(extract$Longitude.decimal)))
extract$Latitude.decimal <- as.numeric((as.character(extract$Latitude.decimal)))

# Definir comme coordonnees projetes
GPS <- as.data.frame(coordinates(extract))


############ Extraire le point moyen de l ensemble des localisations 

mean_x <- mean(GPS$Latitude)
mean_y <- mean(GPS$Longitude)

Tab_MILSAR_mean <- data.frame(c(mean_x), c(mean_y))
colnames(Tab_MILSAR_mean) <- c("Latitude", "Longitude")

write.csv(x = Tab_MILSAR_mean, file = paste(chemin_BDD, "_point_mean.csv", sep =""))


########### Extraire le point median avec le package 

pt_median <- spatial.median(GPS, maxiter = 1000)

Tab_MILSAR_median <- data.frame(pt_median)

Tab_MILSAR_median <- t(Tab_MILSAR_median)

write.csv(x = Tab_MILSAR_median, file = paste(chemin_BDD, "_point_median.csv", sep =""))









############ PREMIERE PARTIE : Traitements des donnees pour les GPS Garmin64S (donnees tracets) ------------

# STEP 1 : Entrer le chemin du fichier GPX a traiter 

chemin_GPX <- paste("D:/Precisions_GPS/Test_9_mise_a_jour/5.60/Piste_2020-05-30 125409_GPS_neuf")

chemin_GPX2 <- paste("D:/Precisions_GPS/Test_9_mise_a_jour/5.60/Piste_2020-05-30 125409_GPS_pas_neuf")
  
sortie <- "D:/Precisions_GPS/Test_9_mise_a_jour/5.60/Piste_2020-05-30 125409"

########### STEP 2 : Extraire donnees GPS des CSV

##### Conversion GPX to CSV (tracets)

gpx_trace <- readGPX(gpx.file = paste(chemin_GPX, ".gpx", sep = ""), waypoints = TRUE)
gpx_trace2 <- readGPX(gpx.file = paste(chemin_GPX2, ".gpx", sep = ""), waypoints = TRUE)

#gpx_point <- readGPX(gpx.file = "H:/Precisions_GPS/Test_1_Waypoints/GPS_1_Neuf/Waypoints_09-MAI-20.gpx", waypoints = TRUE)

gpx_trace <- data.frame(gpx_trace$tracks[[1]])
gpx_trace2 <- data.frame(gpx_trace2$tracks[[1]])
  
# Extraire colonne des coordonnees GPS (Latitude et Longitude)
extract <- gpx_trace[ ,c(1:2)]
colnames(extract) <- c("lon", "lat")

extract2 <- gpx_trace2[ ,c(1:2)]
colnames(extract2) <- c("lon", "lat")

GPX_bind <- rbind(extract, extract2)

write.csv(x = GPX_bind, file = paste(sortie, "_tot.csv", sep =""))


# Impor csv
GPS <-  read.csv(paste(sortie, "_tot.csv", sep =""))

# Definir comme coordonnees projetes
GPS <- as.data.frame(coordinates(extract))





########### STEP 3 : Extraire le point moyen et median de l ensemble des localisations 

##### Extraire le point moyen

mean_x <- mean(GPS[ ,1])
mean_y <- mean(GPS[ ,2])

Tab_Garmin64S_mean <- data.frame(c(mean_x), c(mean_y))
colnames(Tab_Garmin64S_mean) <- c("Longitude", "Latitude")

write.csv(x = Tab_Garmin64S_mean, file = paste(sortie, "_point_mean.csv", sep =""))


##### Extraire le point median 

pt_median <- spatial.median(GPS, maxiter = 1000)

Tab_Garmin64S_median <- data.frame(pt_median)

Tab_Garmin64S_median <- t(Tab_Garmin64S_median)

write.csv(x = Tab_Garmin64S_median, file = paste(sortie, "_point_median.csv", sep =""))



############ PREMIERE PARTIE : Traitements des donnees pour les GPS Garmin64S (donnees waypoints) ----------

# STEP 1 : Entrer les chemins des fichiers GPX a traiter 

chemin_GPX <- "H:/Precisions_GPS/Test_3_Echantillonnage_normal_et_1s/Echantillonnage_1s_pendant_15min_2nd/Piste_2020-05-12 180307_GPS_neuf" # Trace en GPX (sasn .gpx) du GPS nÂ°1

chemin_GPX_2 <- "H:/Precisions_GPS/Test_3_Echantillonnage_normal_et_1s/Echantillonnage_1s_pendant_15min_2nd/Piste_2020-05-12 180337_GPS_pas_neuf" # Trace en GPX (sans .gpx) du GPS nÂ°2

fichier_sortie <- "H:/Precisions_GPS/Test_3_Echantillonnage_normal_et_1s/Echantillonnage_1s_pendant_15min_2nd/Piste_2020-05-12_total" # Nom des fichiers de fusion (csv + mean + median) des waypoints en sortie 



########### STEP 2 : Extraire donnees GPS des CSV

##### Conversion GPX to CSV (tracets)

gpx_trace <- readGPX(gpx.file = paste(chemin_GPX, ".gpx", sep = ""), waypoints = TRUE)
gpx_trace_2 <- readGPX(gpx.file = paste(chemin_GPX_2, ".gpx", sep = ""), waypoints = TRUE)


gpx_trace <- data.frame(gpx_trace$tracks[[1]])
colnames(gpx_trace) <- c("lon", "lat", "ele", "time")
gpx_trace_2 <- data.frame(gpx_trace_2$tracks[[1]])
colnames(gpx_trace_2) <- c("lon", "lat", "ele", "time")

gpx_tot <- rbind(gpx_trace, gpx_trace_2)

write.csv(gpx_tot, file = paste(fichier_sortie, ".csv", sep = ""))




##### Entrer la base de donnee

BDD <- read.csv(paste(fichier_sortie, ".csv", sep = ""))

# Extraire colonne des coordonnees GPS (Latitude et Longitude)
extract <- BDD[ ,c(2:3)]

# Definir comme coordonnees projetes
GPS <- as.data.frame(coordinates(extract))





########### STEP 3 : Extraire le point moyen et median de l ensemble des localisations 

##### Extraire le point moyen

mean_x <- mean(GPS[ ,1])
mean_y <- mean(GPS[ ,2])

Tab_Garmin64S_mean <- data.frame(c(mean_x), c(mean_y))
colnames(Tab_Garmin64S_mean) <- c("Longitude", "Latitude")

write.csv(x = Tab_Garmin64S_mean, file = paste(fichier_sortie, "_point_mean.csv", sep =""))


##### Extraire le point median 

pt_median <- spatial.median(GPS, maxiter = 1000)

Tab_Garmin64S_median <- data.frame(pt_median)

Tab_Garmin64S_median <- t(Tab_Garmin64S_median)

write.csv(x = Tab_Garmin64S_median, file = paste(fichier_sortie, "_point_median.csv", sep =""))









############ PREMIERE PARTIE : Traitement des donnees pour ASUS -------

chemin_csv <- "H:/Precisions_GPS/Test_1_Waypoints/ASUS_GPS_RESEAU_WIFI_BLUETOOTH/Waypoints_09-05-2020"


########### STEP 1 : 

BDD <- read.csv2(paste(chemin_csv, ".csv", sep = ""))

# Extraire colonne des coordonnees GPS (Latitude et Longitude)
extract <- BDD[ ,c(1:2)]

# Definir comme coordonnees projetes
GPS <- as.data.frame(coordinates(extract))





########### STEP 2 : Extraire le point moyen et median de l ensemble des localisations 

##### Extraire le point moyen

mean_x <- mean(GPS[ ,1])
mean_y <- mean(GPS[ ,2])

Tab_Garmin64S_mean <- data.frame(c(mean_x), c(mean_y))
colnames(Tab_Garmin64S_mean) <- c("Latitude", "Longitude")

write.csv(x = Tab_Garmin64S_mean, file = paste(chemin_csv, "_point_mean.csv", sep =""))


##### Extraire le point median 

pt_median <- spatial.median(GPS, maxiter = 1000)

Tab_Garmin64S_median <- data.frame(pt_median)

Tab_Garmin64S_median <- t(Tab_Garmin64S_median)

write.csv(x = Tab_Garmin64S_median, file = paste(chemin_csv, "_point_median.csv", sep =""))



############ PREMIERE PARTIE : Analyse de la precision des balises MILSAR (distance au point de reference)--------


############ STEP 1 : Modifier les chemins ci-dessous avant de lancer le scrip

chemin_BDD <- "D:/Precisions_GPS/Test_4_Balise_MILSAR/Points_tot" # Entrer le chemin du fichier tableur (avec .csv) contenant les points GPS


############ STEP 2 : lancer le script

# Entrer la base de donnee
BDD <- read.csv2(paste(chemin_BDD, ".csv", sep = ""))

data.frame(BDD)

MILSAR_09 <- filter(BDD, Device.ID == 987009)
MILSAR_02 <- filter(BDD, Device.ID == 987002)
MILSAR_08 <- filter(BDD, Device.ID == 987008)

# Extraire colonne des coordonnees GPS (Latitude et Longitude)
extract_09 <- MILSAR_09[ , c("Longitude.decimal", "Latitude.decimal")]
extract_02 <- MILSAR_02[ , c("Longitude.decimal", "Latitude.decimal")]
extract_08 <- MILSAR_08[ , c("Longitude.decimal", "Latitude.decimal")]


# Convertir les coordonnees en numerique
extract_09$Longitude.decimal <- as.numeric((as.character(extract_09$Longitude.decimal)))
extract_09$Latitude.decimal <- as.numeric((as.character(extract_09$Latitude.decimal)))


extract_02$Longitude.decimal <- as.numeric((as.character(extract_02$Longitude.decimal)))
extract_02$Latitude.decimal <- as.numeric((as.character(extract_02$Latitude.decimal)))

extract_08$Longitude.decimal <- as.numeric((as.character(extract_08$Longitude.decimal)))
extract_08$Latitude.decimal <- as.numeric((as.character(extract_08$Latitude.decimal)))
# Calculer la distance au point de referance : -21.08710 - 55.28423 avec l ellipsoide RGR92 ayant comme a et f definit par l IGN

# Pour la balise MILSAR 987009

result_09 <- c()

for (i in 1:nrow(extract_09)){
  dist <- distGeo(c(extract_09[i , 1],extract_09[i , 2]), c(55.28423, -21.08710) , a = 6378137, f =1/298.257223563)
  result_09[i] <- dist
  i <- i+1
}


hist(result_09, freq = T,
     main = "Resultats de la balise MILSAR nÂ°987009",
     ylab = "Nombre de points",
     xlab = "Distance au point de reference (m)")

sum_09 <- summary(result_09)


# Pour la balise MILSAR 987002

result_02 <- c()

for (i in 1:nrow(extract_02)){
  dist <- distGeo(c(extract_02[i , 1],extract_02[i , 2]), c(55.28423, -21.08710) , a = 6378137, f =1/298.257223563)
  result_02[i] <- dist
  i <- i+1
}


result_02 

hist(result_02, freq = T,
     main = "Resultats de la balise MILSAR nÂ°987002",
     ylab = "Nombre de points",
     xlab = "Distance au point de reference (m)")

sum_02 <- summary(result_02)


# Pour la balise MILSAR 987008

result_08 <- c()

for (i in 1:nrow(extract_08)){
  dist <- distGeo(c(extract_08[i , 1],extract_08[i , 2]), c(55.28423, -21.08710) , a = 6378137, f =1/298.257223563)
  result_08[i] <- dist
  i <- i+1
}


hist(result_08, freq = T,
     main = "Resultats de la balise MILSAR nÂ°987009",
     ylab = "Nombre de points",
     xlab = "Distance au point de reference (m)")

sum_08 <- summary(result_08)




# Pour un tapon a 10m on exclus les valeurs extremes

100 - as.numeric(length(result_09))*(sum(result_09 > 10))/100 # 97.34 des valeurs seront dans les 10m pour MILSAR 09 (sur 38 points testes)

100 - as.numeric(length(result_02))*(sum(result_02 > 10))/100 # 98.4 des valeurs seront dans les 10m pour MILSAR 02 (sur 32 points testes)

100 - as.numeric(length(result_08))*(sum(result_08 > 10))/100 # 95.04 des valeurs seront dans les 10m pour MILSAR 02 (sur 32 points testes)



############ PREMIERE PARTIE : Analyse de la precision des GPS (distance au point de reference) -----------

# Integrer les deux bases de donnees avec les waypoints et les fusionner

GPS_BDD1 <- "D:/Precisions_GPS/Test_1_Waypoints/GPS/Waypoints_09-MAI-20_total"
GPS_BDD2 <- "D:/Precisions_GPS/Test_1_Waypoints/GPS/Waypoints_10-MAI-20_total"


BDD1 <- read.csv(paste(GPS_BDD1, ".csv", sep = ""))
BDD2 <- read.csv(paste(GPS_BDD2, ".csv", sep = ""))

extract_BDD1 <- BDD1[ ,c("lon", "lat")]
extract_BDD2 <- BDD2[ ,c("lon", "lat")]

BDD_tot <- rbind(extract_BDD1, extract_BDD2)

# Calculer la distance entre tous les waypoints et le point de reference

result_BDD_tot <- c()

for (i in 1:nrow(BDD_tot)){
  dist <- distGeo(c(BDD_tot[i , 1],BDD_tot[i , 2]), c(55.28423, -21.08710) , a = 6378137, f =1/298.257223563)
  result_BDD_tot[i] <- dist
  i <- i+1
}

result_BDD_tot

hist(result_BDD_tot)

sum_GPS <- summary(result_BDD_tot)


############ PREMIERE PARTIE : Bilan resultat premiere partie MILSAR et GPS -----

bilan <- data.frame(rbind(sum_02, sum_09, sum_08, sum_GPS))

write.csv2(bilan, file = "D:/Precisions_GPS/Test_1_Waypoints/GPS/comparaison_distance_GPS_MILSAR.csv")

############ DEUXIEME PARTIE.1 : Graphique : Precision en fonction du temps -------

################## Chemins et donnees d entrees

Garmin_1 <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures", "Piste_2020-07-04 153939_GPS_neuf_TP3", sep = "/") #Tracer GPX GPS1

Garmin_2 <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures", "Piste_2020-07-04 153939_GPS_pas_neuf_TP3", sep = "/") #Tracer GPX GPS1

sortie <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures")

X <- 55.4774836 #Coordonnees X du point de reference pour calculer la precision (distance pt median au pt reference). En WGS84 epsg = 4326.

Y <- -20.9043701 #Coordonnees Y du point de reference pour calculer la precision (distance pt median au pt reference). En WGS84 epsg = 4326.

echantillonnage <- 20 # Exemple : Si echantillonnage gps = 1 pt/30s toutes les 10 minutes (10 = variable) donc mettre 20 en entre

intervalle <- 10 # Exemple ci-dessus mettre 10 = Temps en minutes que dure un intervalle 


# Explication graphique de sortie : Echantillonnage des GPS de 30s, calcule des points medians toutes les **vars echantillonnage***
# et de la distance de ces points median au point de reference positionne manuellement sur une image satellite de 0.5m de precision


############ DEUXIEME PARTIE.2 : Pour la courbe couplant les deux Garmins -------


################## STEP 1 : Import GPX 


### Import fichier GPX Garmin 1 et garmin 2
GPX1 <- readGPX(paste(Garmin_1, ".gpx", sep = ""))
GPX1_df <- data.frame(GPX1$tracks[[1]])
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time" )

GPX2 <- readGPX(paste(Garmin_2, ".gpx", sep = ""))
GPX2_df <- data.frame(GPX2$tracks[[1]])
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time" )

GPX_bind <- rbind(GPX1_df, GPX2_df)

# Classer dans l ordre chronologique avec la 4eme colonne 
GPX_bind <- GPX_bind[order(GPX_bind[,4], decreasing = FALSE), ]



################# STEP 2 : Segmenter les points de la base pour calculer les points medians


# Creer le vecteur de segmentation du tableau des points GPS
vect_seq <- c(seq(from = 1, to = length(GPX_bind[,1]), by = echantillonnage*2)) # Fois 2 car ajouter point Garmin1 + garmin2

# Coller une colone numero de ligne
indice <- c(seq(from = 1, to = length(GPX_bind[,1]), by = 1))
GPX_bind <- cbind(GPX_bind, indice)
colnames(GPX_bind) <- c("Lon", "Lat", "ele", "time", "Num_ligne")


# Creer le tableau de stockage de sortie de la boucle
BDD_GPX_median <- data.frame(1,2)
colnames(BDD_GPX_median) <- c("Lon", "Lat")


# Calculer les points medians pour chacune des périodes 
# Erreur sur la derniere valeur pas importante, dernier serie de point entre la dernière valeur et n+1 impossible a calculer (normal)
for (i in 1:length(vect_seq)) {
  
  print(i)
  pt_periode <- filter(GPX_bind, GPX_bind$Num_ligne >= vect_seq[i] & GPX_bind$Num_ligne < vect_seq[i+1])
  pt_coord <- pt_periode[ ,c("Lon", "Lat")]
  pt_median <- (spatial.median(pt_coord, maxiter = 1000))
  BDD_GPX_median <- rbind(BDD_GPX_median, pt_median)
}

### Enlever la premiere ligne du dataframe correspondant a la ligne de creation 
BDD_GPX_median <- BDD_GPX_median[-1,]






################# STEP 3 : Calculer la precision gps sur chaque periodes


result_pt_median <- c()

for (i in 1:nrow(BDD_GPX_median)){
  dist <- distGeo(c(BDD_GPX_median[i , 1],BDD_GPX_median[i , 2]), c(X, Y) , a = 6378137, f =1/298.257223563)
  result_pt_median[i] <- dist
  i <- i+1
}


# Transformer en dataframe pour ensuite ploter
result_pt_median_G1G2 <- data.frame(result_pt_median)
colnames(result_pt_median_G1G2) <- c("valeur")

# Ajouter une ligne au dataframe correspondant au temps en minutes
inter_hist <- seq(from = 0, to = length(result_pt_median_G1G2$valeur)*intervalle, by = intervalle)
inter_hist <- inter_hist[-1]

result_pt_median_G1G2 <- cbind(result_pt_median_G1G2, inter_hist)
colnames(result_pt_median_G1G2) <- c("valeur", "minutes")

# Ajouter une colonne specifiant les donnees utilisees
vect <- c("Garmin 1 & 2")
result_pt_median_G1G2 <- cbind(result_pt_median_G1G2, vect)
colnames(result_pt_median_G1G2) <- c("valeur", "minutes", "donnees")

mean(result_pt_median_G1G2$valeur)

############ DEUXIEME PARTIE.3 : Pour la courbe Garmin1 -------

################## STEP 1 : Import GPX 


### Import fichier GPX Garmin 1 et garmin 2
GPX1 <- readGPX(paste(Garmin_1, ".gpx", sep = ""))
GPX1_df <- data.frame(GPX1$tracks[[1]])
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time" )




################# STEP 2 : Segmenter les points de la base pour calculer les points medians


# Creer le vecteur de segmentation du tableau des points GPS
vect_seq <- c(seq(from = 1, to = length(GPX1_df[,1]), by = echantillonnage))

# Coller une colone numero de ligne
indice <- c(seq(from = 1, to = length(GPX1_df[,1]), by = 1))
GPX1_df <- cbind(GPX1_df, indice)
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time", "Num_ligne")


# Creer le tableau de stockage de sortie de la boucle
BDD_GPX_median <- data.frame(1,2)
colnames(BDD_GPX_median) <- c("Lon", "Lat")


# Calculer les points medians pour chacune des périodes 
# Erreur sur la derniere valeur pas importante, dernier serie de point entre la dernière valeur et n+1 impossible a calculer (normal)
for (i in 1:length(vect_seq)) {
  
  print(i)
  pt_periode <- filter(GPX1_df, GPX1_df$Num_ligne >= vect_seq[i] & GPX1_df$Num_ligne < vect_seq[i+1])
  pt_coord <- pt_periode[ ,c("Lon", "Lat")]
  pt_median <- (spatial.median(pt_coord, maxiter = 1000))
  BDD_GPX_median <- rbind(BDD_GPX_median, pt_median)
}

### Enlever la premiere ligne du dataframe correspondant a la ligne de creation 
BDD_GPX_median <- BDD_GPX_median[-1,]






################# STEP 3 : Calculer la precision gps sur chaque periodes


result_pt_median <- c()

for (i in 1:nrow(BDD_GPX_median)){
  dist <- distGeo(c(BDD_GPX_median[i , 1],BDD_GPX_median[i , 2]), c(X, Y) , a = 6378137, f =1/298.257223563)
  result_pt_median[i] <- dist
  i <- i+1
}


# Transformer en dataframe pour ensuite ploter
result_pt_median_G1 <- data.frame(result_pt_median)
colnames(result_pt_median_G1) <- c("valeur")

# Ajouter une ligne au dataframe correspondant au temps en minutes
inter_hist <- seq(from = 0, to = length(result_pt_median_G1$valeur)*intervalle, by = intervalle)
inter_hist <- inter_hist[-1]

result_pt_median_G1 <- cbind(result_pt_median_G1, inter_hist)
colnames(result_pt_median_G1) <- c("valeur", "minutes")

# Ajouter une colonne specifiant les donnees utilisees
vect <- c("Garmin 1")
result_pt_median_G1 <- cbind(result_pt_median_G1, vect)
colnames(result_pt_median_G1) <- c("valeur", "minutes", "donnees")






############ DEUXIEME PARTIE.4 : Pour la courbe Garmin2 -------

################## STEP 1 : Import GPX 


### Import fichier GPX Garmin 1 et garmin 2
GPX2 <- readGPX(paste(Garmin_2, ".gpx", sep = ""))
GPX2_df <- data.frame(GPX2$tracks[[1]])
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time" )




################# STEP 2 : Segmenter les points de la base pour calculer les points medians


# Creer le vecteur de segmentation du tableau des points GPS
vect_seq <- c(seq(from = 1, to = length(GPX2_df[,1]), by = echantillonnage))

# Coller une colone numero de ligne
indice <- c(seq(from = 1, to = length(GPX2_df[,1]), by = 1))
GPX2_df <- cbind(GPX2_df, indice)
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time", "Num_ligne")


# Creer le tableau de stockage de sortie de la boucle
BDD_GPX_median <- data.frame(1,2)
colnames(BDD_GPX_median) <- c("Lon", "Lat")


# Calculer les points medians pour chacune des périodes 
# Erreur sur la derniere valeur pas importante, dernier serie de point entre la dernière valeur et n+1 impossible a calculer (normal)
for (i in 1:length(vect_seq)) {
  
  print(i)
  pt_periode <- filter(GPX2_df, GPX2_df$Num_ligne >= vect_seq[i] & GPX2_df$Num_ligne < vect_seq[i+1])
  pt_coord <- pt_periode[ ,c("Lon", "Lat")]
  pt_median <- (spatial.median(pt_coord, maxiter = 1000))
  BDD_GPX_median <- rbind(BDD_GPX_median, pt_median)
}

### Enlever la premiere ligne du dataframe correspondant a la ligne de creation 
BDD_GPX_median <- BDD_GPX_median[-1,]






################# STEP 3 : Calculer la precision gps sur chaque periodes


result_pt_median <- c()

for (i in 1:nrow(BDD_GPX_median)){
  dist <- distGeo(c(BDD_GPX_median[i , 1],BDD_GPX_median[i , 2]), c(X, Y) , a = 6378137, f =1/298.257223563)
  result_pt_median[i] <- dist
  i <- i+1
}


# Transformer en dataframe pour ensuite ploter
result_pt_median_G2 <- data.frame(result_pt_median)
colnames(result_pt_median_G2) <- c("valeur")

# Ajouter une ligne au dataframe correspondant au temps en minutes
inter_hist <- seq(from = 0, to = length(result_pt_median_G2$valeur)*intervalle, by = intervalle)
inter_hist <- inter_hist[-1]

result_pt_median_G2 <- cbind(result_pt_median_G2, inter_hist)
colnames(result_pt_median_G2) <- c("valeur", "minutes")

# Ajouter une colonne specifiant les donnees utilisees
vect <- c("Garmin 2")
result_pt_median_G2 <- cbind(result_pt_median_G2, vect)
colnames(result_pt_median_G2) <- c("valeur", "minutes", "donnees")




############ DEUXIEME PARTIE.5 : Afficher les resultats des 3 variables sous forme de graphique et les exporter------

### Fusion des trois base de donnee cree par les scripts partie 2, 3, 4
BDD_tot <- result_pt_median_G1                     #Ajout des donnees garmin 1 a la base totale
BDD_tot <- rbind(BDD_tot, result_pt_median_G2)     #Ajout des donnees garmin 2 a la base totale
BDD_tot <- rbind(BDD_tot, result_pt_median_G1G2)   #Ajout des donnees garmin 1 et 2 fusionnee a la base totale


### Ploter le resultat

graph1 <- ggplot(BDD_tot, aes(x = minutes, y = valeur, fill = donnees, colour = donnees)) +
  geom_line()

#graph1 <- graph1 + geom_point()

graph1 <- graph1 + scale_color_manual(values=c('#999999','#E69F00', '#A91101'))

graph1 <- graph1 + xlab("Somme des periodes de temps (minutes)") + ylab("Distance au point de référence (m)") 

graph1 <-  graph1 + theme(legend.position = "bottom", legend.title = element_blank())

graph1 <- graph1 + scale_x_continuous(breaks=seq(0, 700, by = 100))

### Exporter 
pdf(paste(sortie, "Variation_precision_en_fonction_du_temps.pdf", sep = "/"))
print(graph1)     # Graphique 1 --> dans la première page du PDF
dev.off() 








############ TROISIEME PARTIE.1 : Courbe cumulee de precision par ajout de point --------

################## Chemins et donnees d entrees

Garmin_1 <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures", "Piste_2020-07-04 153939_GPS_neuf_TP3", sep = "/") #Trace GPX GPS1

Garmin_2 <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures", "Piste_2020-07-04 153939_GPS_pas_neuf_TP3", sep = "/") #Trace GPX GPS2

sortie <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures")

nom <- "TP3" # Ajouter un nom unique au fichier de sortie (pdf)

X <- 55.4774836 #Coordonnees X du point de reference pour calculer la precision (distance pt median au pt reference). En WGS84 epsg = 4326.

Y <- -20.9043701 #Coordonnees Y du point de reference pour calculer la precision (distance pt median au pt reference). En WGS84 epsg = 4326.


# Explication graphique : Variation de la distance au point de reference par ajout d un point suplementaire au calcule du point median 
# C'est l'erreur (biais + precision)




############ TROISIEME PARTIE.2 : Pour la courbe couplant les deux Garmins-----

################## STEP 1 : Import GPX 


### Import fichier GPX Garmin 1 et garmin 2
GPX1 <- readGPX(paste(Garmin_1, ".gpx", sep = ""))
GPX1_df <- data.frame(GPX1$tracks[[1]])
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time" )

GPX2 <- readGPX(paste(Garmin_2, ".gpx", sep = ""))
GPX2_df <- data.frame(GPX2$tracks[[1]])
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time" )

GPX_bind <- rbind(GPX1_df, GPX2_df)

# Classer les lignes grace a l heure sur la colonne 4
GPX_bind <- GPX_bind[order(GPX_bind[,4], decreasing = FALSE), ]



################# STEP 2 : calculer le points median en ajoutant un point supplementaire a chaque fois


# Coller une colonne numero de ligne utilise dans la boucle
indice <- c(seq(from = 1, to = length(GPX_bind[,1]), by = 1))
GPX_bind <- cbind(GPX_bind, indice)
colnames(GPX_bind) <- c("Lon", "Lat", "ele", "time", "Num_ligne")


### Creer le dataframe de stockage du resultat de la boucle
BDD_median <- data.frame(1, 2)

# Calculer les points median  
# Erreur sur la derniere valeur pas importante, dernier serie de point entre la dernière valeur et n+1 impossible a calculer (normal)

i <- 1
for (i in seq(from = 1, to = length(indice), by = 2)) { #i+2 car ajouter point Garmin1 + garmin2
  
  print(i)

  pt_periode <- filter(GPX_bind, GPX_bind$Num_ligne >= indice[1] & GPX_bind$Num_ligne <= indice[i+1]) 
  pt_coord <- pt_periode[ ,c("Lon", "Lat")]
  pt_median <- (spatial.median(pt_coord, maxiter = 1000))
  BDD_median <- rbind(BDD_median, pt_median)

}



### Supprimer la premiere ligne du dataframe creer lors de la creation du dataframe et le premier point ou le pt median n est pas calcule
BDD_median <- BDD_median[-1, ]



################# STEP 3 : Calculer la precision gps a chaque ajout d'un point

result_pt_median <- c()

for (i in 1:nrow(BDD_median)){
  dist <- distGeo(c(BDD_median[i , 1],BDD_median[i , 2]), c(X, Y) , a = 6378137, f =1/298.257223563)
  result_pt_median[i] <- dist
}



# Transformer en dataframe pour ensuite ploter
result_pt_median_G1G2 <- data.frame(result_pt_median)
colnames(result_pt_median_G1G2) <- c("valeur")



# Ajouter une ligne au dataframe correspondant au nombre de point utilise pour calculer la valeur du pt median sur le graphique
nb_point <- seq(from = 2, to = length(result_pt_median_G1G2$valeur)+1, by = 1)
result_pt_median_G1G2 <- cbind(result_pt_median_G1G2, nb_point)

# Ajouter une colonne pour specifier le type de donnees
vect <- ("Garmin 1 & 2")
result_pt_median_G1G2 <- cbind(result_pt_median_G1G2, vect)





############ TROISIEME PARTIE.3 : Pour la courbe Garmin 1 ------

################## STEP 1 : Import GPX 


### Import fichier GPX Garmin 1 et garmin 2
GPX1 <- readGPX(paste(Garmin_1, ".gpx", sep = ""))
GPX1_df <- data.frame(GPX1$tracks[[1]])
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time" )






################# STEP 2 : calculer le points median en ajoutant un point supplementaire a chaque fois


# Coller une colone numero de ligne utilise dans la boucle
indice <- c(seq(from = 1, to = length(GPX1_df[,1]), by = 1))
GPX1_df <- cbind(GPX1_df, indice)
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time", "Num_ligne")


### Creer le dataframe de stockage du resultat de la boucle
BDD_median <- data.frame(1, 2)

# Calculer les points median  
# Erreur sur la derniere valeur pas importante, dernier serie de point entre la dernière valeur et n+1 impossible a calculer (normal)

for (i in 1:length(indice)) {
  
  print(i)
  
  pt_periode <- filter(GPX1_df, GPX1_df$Num_ligne >= indice[1] & GPX1_df$Num_ligne <= indice[i+1])
  pt_coord <- pt_periode[ ,c("Lon", "Lat")]
  pt_median <- (spatial.median(pt_coord, maxiter = 1000))
  BDD_median <- rbind(BDD_median, pt_median)
  
}


### Supprimer la premiere ligne du dataframe creer lors de la creation du dataframe et le premier point ou le pt median n est pas calcule
BDD_median <- BDD_median[-1, ]





################# STEP 3 : Calculer la precision gps a chaque ajout d'un point

result_pt_median <- c()

for (i in 1:nrow(BDD_median)){
  dist <- distGeo(c(BDD_median[i , 1],BDD_median[i , 2]), c(X, Y) , a = 6378137, f =1/298.257223563)
  result_pt_median[i] <- dist
  i <- i+1
}



# Transformer en dataframe pour ensuite ploter
result_pt_median_G1 <- data.frame(result_pt_median)
colnames(result_pt_median_G1) <- c("valeur")



# Ajouter une ligne au dataframe correspondant au nombre de point utilise pour calculer la valeur du pt median sur le graphique
nb_point <- seq(from = 2, to = length(result_pt_median_G1$valeur)+1, by = 1)
result_pt_median_G1 <- cbind(result_pt_median_G1, nb_point)


# Ajouter une colonne pour specifier le type de donnees
vect <- ("Garmin 1")
result_pt_median_G1 <- cbind(result_pt_median_G1, vect)








############ TROISIEME PARTIE.4 : Pour la courbe Garmin 2 ------

################## STEP 1 : Import GPX 


### Import fichier GPX Garmin 1 et garmin 2
GPX2 <- readGPX(paste(Garmin_2, ".gpx", sep = ""))
GPX2_df <- data.frame(GPX2$tracks[[1]])
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time" )






################# STEP 2 : calculer le points median en ajoutant un point supplementaire a chaque fois


# Coller une colone numero de ligne utilise dans la boucle
indice <- c(seq(from = 1, to = length(GPX2_df[,1]), by = 1))
GPX2_df <- cbind(GPX2_df, indice)
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time", "Num_ligne")


### Creer le dataframe de stockage du resultat de la boucle
BDD_median <- data.frame(1, 2)

# Calculer les points median  
# Erreur sur la derniere valeur pas importante, dernier serie de point entre la dernière valeur et n+1 impossible a calculer (normal)
for (i in 1:length(indice)) {
  
  print(i)
  
  pt_periode <- filter(GPX2_df, GPX2_df$Num_ligne >= indice[1] & GPX2_df$Num_ligne <= indice[i+1])
  pt_coord <- pt_periode[ ,c("Lon", "Lat")]
  pt_median <- (spatial.median(pt_coord, maxiter = 1000))
  BDD_median <- rbind(BDD_median, pt_median)
  
}


### Supprimer la premiere ligne du dataframe creer lors de la creation du dataframe et le premier point ou le pt median n est pas calcule
BDD_median <- BDD_median[-1, ]



################# STEP 3 : Calculer la precision gps a chaque ajout d'un point

result_pt_median <- c()

for (i in 1:nrow(BDD_median)){
  dist <- distGeo(c(BDD_median[i , 1],BDD_median[i , 2]), c(X, Y) , a = 6378137, f =1/298.257223563)
  result_pt_median[i] <- dist
  i <- i+1
}



# Transformer en dataframe pour ensuite ploter
result_pt_median_G2 <- data.frame(result_pt_median)
colnames(result_pt_median_G2) <- c("valeur")



# Ajouter une ligne au dataframe correspondant au nombre de point utilise pour calculer la valeur du pt median sur le graphique
nb_point <- seq(from = 2, to = length(result_pt_median_G2$valeur)+1, by = 1)
result_pt_median_G2 <- cbind(result_pt_median_G2, nb_point)


# Ajouter une colonne pour specifier le type de donnees
vect <- ("Garmin 2")
result_pt_median_G2 <- cbind(result_pt_median_G2, vect)





############ TROISIEME PARTIE.5 : Afficher les resultats sous forme de graphique des 3 variables et les exporter -----


BDD_tot <- result_pt_median_G1
BDD_tot <- rbind(BDD_tot, result_pt_median_G2)
BDD_tot <- rbind(BDD_tot, result_pt_median_G1G2)


# Selectionner les 1000 premiers points pour faire le graphique
#BDD1 <- result_pt_median[1:1001, ]


graph1 <- ggplot(BDD_tot, aes(x = nb_point, y = valeur, fill = vect, colour = vect)) +
  geom_line()


graph1 <- graph1 + xlab("Nombre de points cummulés") + ylab("Distance au point de référence (m)")

graph1 <- graph1 + scale_color_manual(values=c('#999999','#E69F00', '#A91101'))

graph1 <-  graph1 + theme(legend.position = "bottom", legend.title = element_blank())

graph1 <- graph1 + scale_x_continuous(breaks=seq(0, 1500, by = 250))

### Exporter 
pdf(paste(sortie,"/", "Variation_precision_en_fonction_du_nb_pt_", nom, ".pdf", sep = ""))
print(graph1)     # Graphique 1 --> dans la première page du PDF
dev.off() 





############ QUATRIEME PARTIE : Courbe cumulee representant le biais, ploter X et Y --------

# Chemin
Garmin_1 <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures", "Piste_2020-07-04 153939_GPS_neuf_TP3", sep = "/") #Trace GPX GPS1

Garmin_2 <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures", "Piste_2020-07-04 153939_GPS_pas_neuf_TP3", sep = "/") #Trace GPX GPS2

sortie <- paste("D:/Precisions_GPS/Test_10_Graphique_plusieurs_heures")

nom <- "TP3" # Ajouter un nom unique au fichier de sortie (pdf)

X <- 55.4774836 #Coordonnees X du point de reference pour calculer la precision (distance pt median au pt reference). En WGS84 epsg = 4326.

Y <- -20.9043701 #Coordonnees Y du point de reference pour calculer la precision (distance pt median au pt reference). En WGS84 epsg = 4326.




              ## ## ## ## ## ## ## ## ## ## ## ##




# Explication graphique : Ploter le X et le Y pour voir le biais 

#### Import

# Import fichier GPX Garmin 1 : variable 1
GPX1 <- readGPX(paste(Garmin_1, ".gpx", sep = ""))
GPX1_df <- data.frame(GPX1$tracks[[1]])
colnames(GPX1_df) <- c("Lon", "Lat", "ele", "time" )


# Import fichier GPX Garmin 2 : Variable 2
GPX2 <- readGPX(paste(Garmin_2, ".gpx", sep = ""))
GPX2_df <- data.frame(GPX2$tracks[[1]])
colnames(GPX2_df) <- c("Lon", "Lat", "ele", "time" )




#### Recuperation des coordonnees et mise en forme pour les ploter

#### Recuperer les coordonnees du Garmin 1 et ajouter une colonne id
pt_coord_G1 <- GPX1_df[ ,c("Lon", "Lat")]
name <- c("Garmin1")
pt_coord_G1 <- cbind(pt_coord_G1, name)


#### Recuperer les coordonnees du Garmin 1 et ajouter une colonne id
pt_coord_G2 <- GPX2_df[ ,c("Lon", "Lat")]
name <- c("Garmin2")
pt_coord_G2 <- cbind(pt_coord_G2, name)

# Fusion et grouper les donnees pour ploter les deux variables sur le meme graph
pt_coord_G1G2 <- rbind(pt_coord_G1, pt_coord_G2)

# Ajouter les coordonnees du point de reference au tableau
ref <- data.frame(X, Y, "Reference")
colnames(ref) <- c("Lon", "Lat", "name")
pt_coord_G1G2 <- rbind(pt_coord_G1G2, ref )


#### Graphique 
graph1 <- ggplot(pt_coord_G1G2, aes(x = pt_coord_G1G2$Lon, y = pt_coord_G1G2$Lat, fill = name, colour = name)) +
  geom_point()

graph1 <- graph1 + scale_color_manual(values=c('#999999','#E69F00', '#A91101'))

graph1 <-  graph1 + theme(legend.position = "bottom", legend.title = element_blank())

graph1 <- graph1 + xlab("Longitude") + ylab("Latitude")

### Exporter 
pdf(paste(sortie,"/", "Analyse_du_biais_plot_X_Y_", nom, ".pdf", sep = ""))
print(graph1)     # Graphique 1 --> dans la première page du PDF
dev.off() 
