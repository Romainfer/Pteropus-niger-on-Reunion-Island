###################################################################################################################################################
#                                                                                                                                                 #
#                                 Etude de l'ecologie de la Roussette Noire (Pteropus niger) sur l Ile de La Reunion                              #
#                                                                                                                                                 #
#                                     -- Script de classification non supervisee de la courronne des arbres  --                                   #
#                                                                                                                                                 #               
#                                                         ------ SCRIPT PARTIE 3.2 ------                                                         #
#                                                                                                                                                 #
#                                                           Romain FERNANDEZ - Juin 2020                                                          #
#                                                                                                                                                 #
###################################################################################################################################################

install.packages("nngeo")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("dbplyr")
install.packages("glcm")
install.packages("caret")
install.packages("JLutils")
install.packages("corrplot")
install.packages("Rmixmod")
install.packages("RColorBrewer")
install.packages("colorRamps")
install.packages("aricode")


library(sp)
library(raster)
library(rgdal)
library(nngeo) #Extraction valeur moyenne polygones
library(reshape2) #Fonction melt
library(ggplot2)
library(dbplyr)
library(sf)
library(tidyr)
library(glcm) #Calcul texture
library(rgrass7)
library(caret) #Pour SVM et partition du dataset avant classification
library(corrplot) #Pour ploter la matrice de correlation
library(pROC) # Courbe ROC 
library(gridExtra) # Export tableau en pdf
library(Rmixmod)
library(RColorBrewer) # palette couleur pour ggplot
library(e1071) #autre package pour le SVM
library(colorRamps) # creer des palettes couleurs avec beaucoup de nuance visibles
library(grid)
library(aricode)

install.packages("ggpmisc")
library(ggpmisc)

################################## !! STEP 1.0 : Chemins creation du dataset------

# Chemins
dossier <- "D:/Donnees Terrain/Dalles" #Racine du chemin la ou sont situe tous les fichiers terrains .csv a fusionner

sortie <- "D:/Output_R_disque/Classification/Classification_non_supervisee/Finale"

buf <- 1 #Buffer en metres appliqué au points GPS pour extraire la signature spectrale des arbres



################################## STEP 1.1 : Creation du dataset : Des fiches terrain de chaque dalle vers une base globale ------


# ATTENTION --> Homogeneiser la base : enlever les fautes sur la colonne nom latin du dataset avant cette etape


### Recuperation de chaque fiche de terrain et regroupement en une seule base de donnee
files <- list.files(path = dossier, pattern = "BDD_finale_dalle", recursive = TRUE, full.names = TRUE) # recuperer les nom des fichiers


# Ouverture des fichiers et fusion dans une base globale
for (i in 1:length(files)){
  
  if(i == 1){
    tab <- read.csv(files[i])
    dataset <- tab
    
  } else {
    tab <- read.csv(files[i])
    dataset <- rbind(dataset, tab)
  }
  
}


### Supprimer les especes notees comme "Inconnue"
dataset <- mutate(dataset, id = rownames(dataset)) # Ajout colonne numero de ligne (id)

extract <- subset(dataset, Espece.latin == "Inconnue") # Extraire num ligne de sobs a suprimer
extract$id <- as.numeric(extract$id)

dataset <- dataset[-c(extract$id) , ]

# Suprimer colonne numero de ligne ajoutee (en derniere position dans dataframe)
col <- length(colnames(dataset))
dataset <- dataset[ ,-col]


write.csv(dataset, file = paste(sortie, "Dataset_non_filtre_totale.csv", sep = "/"))







################################## STEP 1.2 : Creation du dataset: De la base globale aux polygones labelises (buffer autour des points) ------


### Transformer le dataset .csv shapefile
dataset <- read.csv(file = paste(sortie, "Dataset_non_filtre_totale.csv", sep = "/"), header = TRUE)

# Supprimer les ligne sans coordonnées qui ne sont donc pas des arbres utilises dans le dataset
dataset <- drop_na(dataset, Longitude) 



### Appliquer un buffer aux points GPS
dataset <- data.frame(dataset)

# Transformation en sf object avec des coordonnees projetes (sinon fonction buffer apres ne fonctionne pas)
dataset <- st_as_sf(dataset, coords = c("Longitude", "Latitude")) # Precision colonne avec coordonnees et projection

# Appliquer le buffer autour des points GPS
dataset <- st_buffer(dataset, dist = buf*10^-5)



### Mettre le tableau sous forme de deux colonnes avec une = esp latin et l'autre = esp vernaculaire
dataset <- subset(dataset, select = c(Espece.latin, Espece.vernaculaire))



### Exportation et projection du nouveau fichier issue du fichie csv
dataset <- as_Spatial(dataset) #Transformer le sf object en SpatialPointDataframe pour exporter

proj4string(dataset) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # Attribuer systeme de coordonnees de reference (celui dans lequel sont les pt gps du tableau)

dataset <- spTransform(dataset, crs("+init=epsg:2975")) # Transformer la projection en celle qui nous convient pour la suite

writeOGR(obj = dataset,
         dsn = sortie, 
         layer = paste("Dataset_training_buffer", "_", buf, "m", sep =""),
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)






################################## !! STEP 2 : Redimensionner l emprises des variables pour la classification ------
# INFORMATIONS : Necessaire que les variables aient la meme emprise pour pourvoir les fusionner en stack object dans la suite
# ATTENTION : Image multispectrale et couche NDVI doivent avoir la meme resolution pour apres etre fusionne en stack object plus loin

# image_multispectrale <- "D:/Pleiades/Traitement_Pansharpening/Zone_etude_fusionnee_balance_couleur.tif"
# 
# couche_ndvi <- "E:/Output_R/Zone_etude_fusionnee_NDVI.tif"
# 
# # Emprise de la nouvelle zone (EPSG:2975)
# Xmin <- 355969
# Ymin <- 7669857
# Xmax <- 366724
# Ymax <- 7678534
# 
# sortie_new_image <- "D:/Output_R_disque/Classification/Zone_reduite"
# 
# 
# 
# 
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# 
# 
# #### Pour l image Pleiade
# raster_a_decouper <- stack(image_multispectrale)
# raster_a_decouper_b1 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.1
# raster_a_decouper_b2 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.2
# raster_a_decouper_b3 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.3
# raster_a_decouper_b4 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.4
# 
# # Determiner l extension pour couper le raster
# ext <- raster(xmn = Xmin, xmx = Xmax, ymn = Ymin, ymx = Ymax)
# 
# # Attribuer la projection voulu aux bandes de l image multispectrale
# crs(raster_a_decouper_b1) <- "+init=epsg:2975 +proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# crs(raster_a_decouper_b2) <- "+init=epsg:2975 +proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# crs(raster_a_decouper_b3) <- "+init=epsg:2975 +proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# crs(raster_a_decouper_b4) <- "+init=epsg:2975 +proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# 
# 
# # Couper chaque bande en fonction de l extension definie
# raster_crop_b1 <- crop(raster_a_decouper_b1, ext)
# raster_crop_b2 <- crop(raster_a_decouper_b2, ext)
# raster_crop_b3 <- crop(raster_a_decouper_b3, ext)
# raster_crop_b4 <- crop(raster_a_decouper_b4, ext)
# 
# # Fusionner les bandes avec la nouvelles emprise
# raster_crop_tot <- stack(raster_crop_b1, raster_crop_b2, raster_crop_b3, raster_crop_b4)
# 
# # Enregistrer le raster
# writeRaster(x = raster_crop_tot, filename = paste(sortie_new_image,"Zone_etude_fusionnee_balance_couleur_extent_pour_classif.tif", sep = "/"))
# 
# 
# 
# 
# 
# #### Pour l indice NDVI
# raster_a_decouper <- raster(couche_ndvi)
# 
# # Determiner l extension pour rogner le raster
# ext <- raster(xmn = Xmin, xmx = Xmax, ymn = Ymin, ymx = Ymax)
# 
# # Attribuer la projection de notre raster a decouper au raster definissant l extension 
# projection(ext) <- proj4string(ext) <- proj4string(raster_a_decouper)
# 
# # Couper le raster en fonction de l extension definie
# raster_crop <- crop(raster_a_decouper, ext)
# 
# # Enregistrer le raster
# writeRaster(x = raster_crop, filename = paste(sortie_new_image, "Zone_etude_fusionnee_NDVI_0.25_extent_pour_classif.tif", sep = "/"))
# 
# 
# 
# 


################################## !! STEP 3 : Calcule de la texture sur image Pleiades redimensionnee ------

# Chemins
image_multispectrale_classif <- "D:/Output_R_disque/Classification/Zone_reduite/Zone_etude_fusionnee_balance_couleur_extent_pour_classif.tif"

sortie_new_image <- "D:/Output_R_disque/Classification/Zone_reduite"

num <- 3 # Numero de l indice de texture dans le nom du fichier de sortie

wind <- c(13,13) # fenetre de calcul de la texture dans fonction glcm



## ## ## ## ## ## ## ## ## ## ## ## ## ##



# Import de la couche proche-infrarouge pour le calcule de la texture
img_mltiSpect <- stack(image_multispectrale_classif)
proj4string(img_mltiSpect) <- CRS("+init=epsg:2975")
band_nir <- img_mltiSpect$Zone_etude_fusionnee_balance_couleur_extent_pour_classif.4

# Calcul de la texture

indice_texture <- glcm(band_nir,
                       window = wind,
                       statistics = c("mean", "variance", "homogeneity", "contrast","entropy"))

names(indice_texture) <- c("mean", "variance", "homogeneity", "contrast","entropy") #Renomer les colonnes dans le stack object


# Exporter les indices de textures en stack object
writeRaster(x = indice_texture, filename = paste(sortie_new_image, paste("Indices_texture_", num, ".tif", sep = ""), sep = "/"))





################################## !! STEP 4.0 : Chemin pour la classification non supervisee ------

# Chemins

couche_ndvi <- "D:/Output_R_disque/Classification/Zone_reduite/Zone_etude_fusionnee_NDVI_0.25_extent_pour_classif.tif"

image_multispectrale <- "D:/Output_R_disque/Classification/Zone_reduite/Zone_etude_fusionnee_balance_couleur_extent_pour_classif.tif"

num <- 1 # Numero de l indice de texture dans le nom du fichier de sortie

indices_texture <- paste("D:/Output_R_disque/Classification/Zone_reduite",
                         paste("Indices_texture_", num, ".tif", sep = ""), sep = "/") # Stack oject avec le/les indice(s) de texture
                                                                            
                                                                            
chemin_dataset <-"D:/Output_R_disque/Classification/Classification_non_supervisee/Finale" #Sans "/" a la fin. Dataset contenant les polygones d entrainement et de verification

buf <- 1 #Buffer en metres appliqué au points GPS pour extraire la signature spectrale des arbres

nom_fichier_dataset <- paste("Dataset_training_buffer", "_", buf, "m", sep ="") #Pas d extension (fichier shapefile)
# ATTENTION !! : dataset doit avoir en 3 premieres colonnes : X (Numerique = id), Espc_lat (nom latin esp), Espc_vr (nom vernaculaire esp)
# Apres ces trois premieres colonnes il doit y avoir les predicteurs

sortie <- "D:/Output_R_disque/Classification/Classification_non_supervisee/Finale"


vars_non_corr <- c("NDVI", "Blue_band", 
                   "mean", "contrast", "entropy") 
# vecteur des noms de variables non correle conserve pour le modele  -> cf resultat matrice correlation
# Utiliser les noms bien orthographies des predicteurs de la matrice de correlation

nb_esp <- 1 # Effectif a partir du quel l espece est retenue dans la classification par le modele





################################## STEP 4.1 : Importation des variables en stack object ------

### Import des predicteurs et associer la meme projection (epsg = 2975)
NDVI <-  stack(couche_ndvi)
proj4string(NDVI) <- CRS("+init=epsg:2975")

img_mltiSpect <- stack(image_multispectrale)
proj4string(img_mltiSpect) <- CRS("+init=epsg:2975")

texture <- stack(indices_texture)
proj4string(texture) <- CRS("+init=epsg:2975")
names(texture) <- c("mean", "variance", "homogeneity", "contrast","entropy")

### Transformation des couches predictrices en object stack
variables <- addLayer(NDVI, img_mltiSpect, texture)
names(variables) <- list("NDVI", "Blue_band", "Green_band", "Red_band", "Nir_band", "mean", "variance", "homogeneity", "contrast","entropy")







################################## STEP 4.2 : Extraire les valeurs des predicteurs par pixel de chaque polygones du dataset ----

### Import du dataset avec polygones labelises par noms des especes
dataset_terrain <- read_sf(dsn = chemin_dataset, layer = nom_fichier_dataset)

# Verifications presence de projection et autres
summary(dataset_terrain)
# str(dataset)

### Extraire la valeurs des pixels par polygone et les noms des esp sous forme de vecteur
dataset_vars <- raster::extract(variables, dataset_terrain, sp = TRUE)

espece <- c(dataset_terrain[ ,1])


# Initialisation des variables de la boucle
df_esp <- data.frame()
df_tot <- data.frame()

for (i in 1:length(dataset_vars)){
  df <- data.frame(dataset_vars[i])
  Espc_lt <- espece$Espc_lt[i]
  id <- i
  df_esp <- cbind(id, Espc_lt, df)
  df_tot <- rbind(df_esp, df_tot)
}

df_tot[-1, ] # Enlever colonne X (=ID, inutile)

print(df_tot)


### Export
write.csv2(df_tot, file = paste(sortie, "Dataset_predicteurs_par_pixel.csv", sep = "/"))




################################## STEP 4.4 : Visualisation variables sur tout jeu de donnees d entrainement en fonction des especes arbres------

dataset_vars <- read.csv2(paste(sortie, "Dataset_predicteurs_par_pixel.csv", sep = "/"))
# -> Probleme : Faire une moyenne de tous les pixels par predicteurs et par espece -> faisable dans ggplot direct ?
# Ne pas utiliser fichier de la classif supervisee

####### Plot de la signature spectrale moyenne de chaque classe -----

# Graphique bande bleue
tab_sign_blue <- subset(dataset_vars, select = c("Espc_lt", "Blue_band"))

tab_sign_blue <- data.frame(tab_sign_blue)

graph1 <- ggplot(tab_sign_blue, aes(x = Espc_lt, y = Blue_band)) +
  geom_boxplot()

graph1 <- graph1  + xlab("Especes") + ylab("Valeur spectrale bande bleue")

graph1 <-  graph1 + coord_flip()


# Graphique bande verte
tab_sign_green <- subset(dataset_vars, select = c("Espc_lt", "Green_band"))

tab_sign_green <- data.frame(tab_sign_green)

graph2 <- ggplot(tab_sign_green, aes(x = Espc_lt, y = Green_band)) +
  geom_boxplot()

graph2 <- graph2  + xlab("Especes") + ylab("Valeur spectrale bande verte")

graph2 <-  graph2 + coord_flip()


# Graphique bande rouge
tab_sign_red <- subset(dataset_vars, select = c("Espc_lt", "Red_band"))

tab_sign_red <- data.frame(tab_sign_red)

graph3 <- ggplot(tab_sign_red, aes(x = Espc_lt, y = Red_band)) +
  geom_boxplot()

graph3 <- graph3  + xlab("Especes") + ylab("Valeur spectrale bande rouge")

graph3 <-  graph3 + coord_flip()




# Graphique bande proche infra-rouge
tab_sign_nirRed <- subset(dataset_vars, select = c("Espc_lt", "Nir_band"))

tab_sign_nirRed <- data.frame(tab_sign_nirRed)

graph4 <- ggplot(tab_sign_nirRed, aes(x = Espc_lt, y = Nir_band)) +
  geom_boxplot()

graph4 <- graph4  + xlab("Especes") + ylab("Valeur spectrale bande infrarouge")

graph4 <-  graph4 + coord_flip()



# Graphique bande NDVI

tab_sign_ndvi <- subset(dataset_vars, select = c("Espc_lt", "NDVI"))

tab_sign_ndvi <- data.frame(tab_sign_ndvi)

graph5 <- ggplot(tab_sign_ndvi, aes(x = Espc_lt, y = NDVI)) +
  geom_boxplot()

graph5 <- graph5  + xlab("Especes") + ylab("Valeur spectrale bande NDVI")

graph5 <-  graph5 + coord_flip()





####### Graphique des bandes textures -----


# Indice mean
tab_sign_tex_mean <- subset(dataset_vars, select = c("Espc_lt", "mean"))

tab_sign_tex_mean <- data.frame(tab_sign_tex_mean)

graph6 <- ggplot(tab_sign_tex_mean, aes(x = Espc_lt, y = tab_sign_tex_mean$mean)) +
  geom_boxplot()

graph6 <- graph6  + xlab("Especes") + ylab("Indice de texture type moyenne")

graph6 <-  graph6 + coord_flip()


# Indice variance
tab_sign_tex_varia <- subset(dataset_vars, select = c("Espc_lt", "variance"))

tab_sign_tex_varia <- data.frame(tab_sign_tex_varia)

graph7 <- ggplot(tab_sign_tex_varia, aes(x = Espc_lt, y = tab_sign_tex_varia$variance)) +
  geom_boxplot()

graph7 <- graph7  + xlab("Especes") + ylab("Indice de texture type variance")

graph7 <-  graph7 + coord_flip()


# indice homogeneity
tab_sign_tex_homo <- subset(dataset_vars, select = c("Espc_lt", "homogeneity"))

tab_sign_tex_homo <- data.frame(tab_sign_tex_homo)

graph8 <- ggplot(tab_sign_tex_homo, aes(x = Espc_lt, y = tab_sign_tex_homo$homogeneity)) +
  geom_boxplot()

graph8 <- graph8  + xlab("Especes") + ylab("Indice de texture type homogeneite")

graph8 <-  graph8 + coord_flip()



# Indice contrast
tab_sign_tex_cont <- subset(dataset_vars, select = c("Espc_lt", "contrast"))

tab_sign_tex_cont <- data.frame(tab_sign_tex_cont)

graph9 <- ggplot(tab_sign_tex_cont, aes(x = Espc_lt, y = tab_sign_tex_cont$contrast)) +
  geom_boxplot()

graph9 <- graph9  + xlab("Especes") + ylab("Indice de texture type contrast")

graph9 <-  graph9 + coord_flip()



# Indice entropy
tab_sign_tex_entro <- subset(dataset_vars, select = c("Espc_lt", "entropy"))

tab_sign_tex_entro <- data.frame(tab_sign_tex_entro)

graph10 <- ggplot(tab_sign_tex_entro, aes(x = Espc_lt, y = tab_sign_tex_entro$entropy)) +
  geom_boxplot()

graph10 <- graph10  + xlab("Especes") + ylab("Indice de texture type entropy")

graph10 <-  graph10 + coord_flip()







####### Graphique nombre d echantillon par classe ----

# Ajouter une colone de 1 dans le dataframe et la sommer
dataset_terrain <- read_sf(dsn = chemin_dataset, layer = nom_fichier_dataset)

dataset_vars_freq <- data.frame(dataset_terrain)
Effectif <- 1
dataset_vars_freq <- cbind(dataset_vars_freq, Effectif)

dataset_vars_freq <- aggregate.data.frame(dataset_vars_freq$Effectif, 
                                          by = list(dataset_vars_freq$Espc_lt), 
                                          FUN = sum, 
                                          na.rm = TRUE) # Sommer la colonne ajouter pour chaque variable

colnames(dataset_vars_freq) <-  c("Espc_lt", "Effectif")

graph11 <- ggplot(dataset_vars_freq, aes(x = Espc_lt, y = Effectif)) +
  geom_bar(stat = "identity")

graph11 <-  graph11 + xlab("Espèces") + ylab("Nombre d'arbres")

graph11 <- graph11 + scale_y_continuous(breaks = seq(0, max(dataset_vars_freq$Effectif) + 1, 2))

graph11 <-  graph11 + coord_flip()




####### Export de tous les graphiques de cette partie en pdf ------

pdf(paste(sortie,"/", "Analyse_dateset_training", ".pdf", sep = ""))
print(graph11)
print(graph1)
print(graph2)
print(graph3) 
print(graph4)
print(graph5)
print(graph6)
print(graph7)
print(graph8)
print(graph9)
print(graph10)
dev.off() 






################################## STEP 4.5 : Matrice de correlation ----

dataset_vars <- read.csv2(paste(sortie, "Dataset_predicteurs_par_pixel.csv", sep = "/"))


### Matrice de correlation

# Supprimer les 3 premieres colonnes (X, ID, esp latin) pour garder que les variables 
dataset_vars <- dataset_vars[ ,-1]
dataset_vars <- dataset_vars[ ,-1]
dataset_vars <- dataset_vars[ ,-1]

# Calculer la matrice
matrix_corr1 <- cor(dataset_vars, method = c("pearson"))




### Mise en forme pour la ploter

# Arrondir les donnees pour afficher dans le plot
matrix_corr <- round(matrix_corr1, 2) 


# Obtenir le triangle supérieur et mettre NA a la place des valeurs inferieures : Creert fonction et l appliquer
# get_upper_tri <- function(matrix){
#   matrix[lower.tri(matrix)] <- NA
#   return(matrix)
# }
# 
# matrix_corr <- get_upper_tri(matrix = matrix_corr)


# Remise en forme de la matrice pour utilisation dans ggplot
matrix_corr_plot <- melt(matrix_corr) 




### Ploter la matrice
ggheatmap <- ggplot(data = matrix_corr_plot, aes(x=Var1, y=Var2, fill=value)) 

ggheatmap <- ggplot(matrix_corr_plot, aes(Var2, Var1, fill = value)) # Double car permet afficher couleur sur moitier matrice

ggheatmap <-  ggheatmap + geom_tile(color = "black") #Lignes noires entre carres matrice

ggheatmap <-  ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) #Valeurs precises dans carres

ggheatmap <- ggheatmap + scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                              midpoint = 0, limit = c(-1,1), space = "Lab",
                                              name="Pearson\nCorrelation")

ggheatmap <- ggheatmap + theme_minimal() # enlever les bordures de la matrice

ggheatmap <- ggheatmap + theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                                          size = 9, hjust = 1)) 

ggheatmap <- ggheatmap + coord_fixed() # Reduit la taille des carres

ggheatmap <- ggheatmap  + xlab("Variable 1") + ylab("Variable 2")


# Exportation
pdf(paste(sortie,"/", "Matrice de correlation", ".pdf", sep = ""))
print(ggheatmap)
dev.off() 





# Correlation = Person > 0.8
high_corr <- findCorrelation(matrix_corr1, cutoff = 0.8)

# Nombre de variable correlee
length(high_corr)

# Nom des variables correlee a enlever du jeu de donnees
vars_corr <- names(dataset_vars)[high_corr]






################################## STEP 4.6 : Filtrer la base de donnees ----

dataset_vars <- read.csv2(paste(sortie, "Dataset_predicteurs_par_pixel.csv", sep = "/"))


### Garder uniquement esp latin 
dataset_vars <- dataset_vars[ , -1] # Enlever colone X (=ID)



### Extraire les variables qui ne sont pas correlees du dataset
dataset_vars_non_cor <- dataset_vars[ ,1:2]

for (i in 1:length(vars_non_corr)){
  dataset_vars1 <- dplyr::select(dataset_vars, select = vars_non_corr[i])
  dataset_vars_non_cor <- cbind(dataset_vars_non_cor, dataset_vars1)
}

colnames(dataset_vars_non_cor) <- c("ID", "Espc_lt", vars_non_corr)

dataset_vars <- dataset_vars_non_cor
remove(dataset_vars_non_cor)




### Enlever les especes a faible effectif 
dataset_vars_freq <- read_sf(dsn = chemin_dataset, layer = nom_fichier_dataset)
Effectif <- 1
dataset_vars_freq <- cbind(dataset_vars_freq, Effectif)

dataset_vars_freq <- aggregate.data.frame(dataset_vars_freq$Effectif, 
                                          by = list(dataset_vars_freq$Espc_lt), 
                                          FUN = sum, 
                                          na.rm = TRUE) # Sommer la colonne ajouter pour chaque variable

colnames(dataset_vars_freq) <-  c("Espc_lt", "Effectif")


# Extraire les esp a fort effectif sous forme de vecteur
esp_classif <- subset(dataset_vars_freq, Effectif >= nb_esp) 
esp_classif <- droplevels(esp_classif)
eff_classif <- esp_classif$Effectif # Pour plus tard dans le script
esp_classif <- esp_classif$Espc_lt


# Conserver uniquement les especes a fort effectif, extraite ci-dessus, dans la BDD
dataset_classif <- data.frame()

for (i in 1:length(esp_classif)){
  dataset_classif1 <- subset(dataset_vars, Espc_lt == esp_classif[i])
  dataset_classif <- rbind(dataset_classif, dataset_classif1)
}


dataset_classif <- droplevels(dataset_classif) # Quand on filtre par des facteur on conserve l ensemble des facteurs 
# du tabelau qui faut ensuite supprimer.
dataset_vars <- dataset_classif
remove(dataset_classif)
remove(dataset_classif1)

print(dataset_vars)

dataset_vars <- dataset_vars[-1] #Enlever la premiere colonne ID





################################## STEP 4.7 : CLASIF NON SUPERVISEE -----

### Classification ----
classif <- mixmodCluster(dataset_vars[2:ncol(dataset_vars)], 
                         nbCluster = c(35:35)) #c(3:(length(esp_classif)))) 

summary(classif)


### Recuperer les clusters dans lesquelles les pixels ont ete classes et creer une nouvelle colonne dans la base ----
cluster <- classif@bestResult@partition 

tab_classif <- cbind(dataset_vars, cluster)


### Recuperer les probas (matrice) d appartenance aux differents clusters pour chaque pixel classes et sommer proba par cluster pour meme esp ----

proba <- data.frame(classif@bestResult@proba)    # Extraction proba du modele
tab_proba <- cbind(dataset_vars$Espc_lt, proba)  

col_vect <- colnames(tab_proba)                   
col_vect <- col_vect[2:length(col_vect)]

esp_vect <- unique(tab_proba$`dataset_vars$Espc_lt`)

matrice_finale <- data.frame()

for (i in 1:length(esp_vect)){
  extract <- subset(x = tab_proba, subset = tab_proba$`dataset_vars$Espc_lt` == esp_vect[i] )
  sum_extract <- colSums(extract[, -1])
  sum_extract <- data.frame(sum_extract)
  colnames(sum_extract) <- esp_vect[i]
  
  nb_row <- nrow(extract)
  sum_extract <- t(sum_extract)
  sum_extract <- sum_extract/nb_row
  matrice_finale <- rbind(matrice_finale, sum_extract)
  
}

print(matrice_finale)


# Renomer les colonnes du tableau (Xn -> Cluster n)
vect_clus <- c()

for (i in 1:ncol(matrice_finale)){
  vect_clus[i] <- paste("Cluster", i, sep = "-")
}

colnames(matrice_finale) <- c(vect_clus)


# Ploter la matrice de probabilite
matrice_finale <- as.matrix(matrice_finale)


pdf(paste(sortie, "/", "Matrice_de_probabilites", ".pdf", sep = ""), height=10, width=15)
 

heatmap <- heatmap(matrice_finale)

legend(x="topleft", legend=c("0", "0.5", "1"), 
       fill=colorRampPalette(brewer.pal(10, "Oranges"))(3))

dev.off()



### Calculer l indice NMI manuellement (erreur dans cette partie de code !!) ------

# tab1 <- tab_classif$Espc_lt
# tab1 <- data.frame(tab1)
# tab2 <- tab_classif$cluster
# 
# tab_NMI <- cbind(tab1, tab2)
# colnames(tab_NMI) <- c("Espc_lt", "Cluster")
# 
# 
# 
# 
# 
# #### Entropy des classes : P(Y=1, 2, 3 etc...)
# esp_vect <- as.character(unique(tab_NMI$Espc_lt))
# 
# # Nombre pixel par espece
# result2 <- data.frame()
# 
# for (i in 1:length(esp_vect)){ 
#   espece <- subset(x = tab_NMI, subset = tab_NMI$Espc_lt == esp_vect[i])
#   nrow_par_esp <- nrow(espece)
#   result <- cbind(nrow_par_esp, esp_vect[i])
#   result2 <- rbind(result2, result)
#   }
# 
# nb_obs_tot <- nrow(tab_NMI)
# 
# vect_result2 <- as.numeric((as.character(result2$nrow_par_esp)))/nb_obs_tot # Nombre de pixel par esp divise par le tot de pixel
# 
# for (i in seq(from = 1, to = length(vect_result2), by = 2 )){
#   if (i == 1) {
#     
#     initial <- - vect_result2[i]*log(vect_result2[i]) 
#     H_Y <- initial - vect_result2[i+1]*log(vect_result2[i+1])
#   }
#   
#   else (H_Y <- H_Y - vect_result2[i]*log(vect_result2[i])
#         )
# }
# 
# print(H_Y)
# 
# 
# 
# 
# 
# #### Entropy des clusters : P(C=1, 2, 3 etc...)
# cluster_vect <- as.character(unique(tab_NMI$Cluster))
# 
# result2 <- data.frame()
# 
# for (i in 1:length(cluster_vect)){ 
#   clus <- subset(x = tab_NMI, subset = tab_NMI$Cluster == cluster_vect[i])
#   nrow_par_cluster <- nrow(clus)
#   result <- cbind(nrow_par_cluster, paste("Cluster-", cluster_vect[i], sep = ""))
#   result2 <- rbind(result2, result)
# }
# 
# nb_obs_tot <- nrow(tab_NMI)
# 
# vect_result2 <- as.numeric((as.character(result2$nrow_par_cluster)))/nb_obs_tot # Nombre de pixel par cluster divise par le tot de pixel
# 
# for (i in seq(from = 1, to = length(vect_result2), by = 2 )){
#   if (i == 1) {
#     
#     initial <- - vect_result2[i]*log(vect_result2[i]) 
#     H_C <- initial - vect_result2[i+1]*log(vect_result2[i+1])
#   }
#   
#   else (H_C <- H_C - vect_result2[i]*log(vect_result2[i])
#   )
# }
# 
# print(H_C)
# 
# 
# 
# ##### Entropy conditionnelle des classes pour les différents clusters : H(Y|C)
# 
# r2 <- data.frame()
# 
# for (i in 1:length(cluster_vect)){
#   
#   
#     clus <- subset(x = tab_NMI, subset = tab_NMI$Cluster == cluster_vect[i])
#     num_cluster <- cluster_vect[i]
#     nrow_clus <- nrow(clus)
#     esp_vect <- as.character(unique(clus$Espc_lt))
#     
#   for(i in 1:length(esp_vect)){
#     esp_clus <- subset(x = clus, subset = clus$Espc_lt == esp_vect[i])
#     nrow_esp_par_cluster <- nrow(esp_clus)
#     r <- cbind(nrow_esp_par_cluster, esp_vect[i],  num_cluster, nrow_clus)
#     r <- data.frame(r)
#     r2 <- rbind(r2, r)
#     }  
# }   
# 
# 
# print(r2)
# 
# 
# # Ajouter une colonne a r2 nb esp dans un cluster divise par nombre esp tot dans le cluster
# division <- as.numeric(as.character(r2$nrow_esp_par_cluster))/as.numeric(as.character(r2$nrow_clus))
# 
# r2 <- cbind(r2, division)
# 
# 
# # Calculer pour chaque cluster l indice H(Y|C)
# 
# r_fin <- data.frame()
# 
# for (i in 1:length(cluster_vect)){
#   
#   print(cluster_vect[i])
#   
#   clus <- subset(x = r2, subset = r2$num_cluster == cluster_vect[i])
#   num_clus <- cluster_vect[i]
#   vect_divi <- clus$division
#   
#   for (j in seq(from = 1, to = length(vect_divi), by = 2 )){
#     
#     if (length(vect_divi) == 1){
#       calcul2 <- (vect_divi[j] * log(vect_divi[j]))
#       print("1")
#       
#     } else {
#       calcul <- (vect_divi[j] * log(vect_divi[j]))
#       if (is.na(vect_divi[j+1]) != TRUE){
#         calcul2 <- calcul + (vect_divi[j+1] * log(vect_divi[j+1]))
#       } else{calcul <- calcul2}
#       
#       print("+1")
#     }
#     
#     calcul3 <- -1/2 * calcul2
# 
#   }
#     print("Ajout result tab")
#     r_cal <- cbind(calcul3, num_clus)
#     r_cal <- data.frame(r_cal)
#     r_fin <- rbind(r_fin, r_cal)
#        
# }
# 
# r_fin <- data.frame(r_fin)
# 
# print(r_fin)
# 
# ##### Calcule de I(Y ; C) = H(Y) - H(Y|C)
# 
# I <- H_Y - (sum(as.numeric(as.character(r_fin$calcul3))))
# 
# NMI <- (2*I)/(H_Y + H_C)
# 
# print(NMI)
# 
# result_NMI <- cbind(NMI, length(unique(tab_classif$cluster)))
# colnames(result_NMI) <- c("Indice NMI", "Nombre de clusters")
# 
# write.csv2(x = result_NMI, file = paste(sortie, "Indice_NMI.csv", sep="/"))



### Calculer l indice NMI package aricode ------

ind_NMI <- NMI(tab_classif$cluster, tab_classif$Espc_lt)

print(ind_NMI)

result_NMI <- cbind(ind_NMI, length(unique(tab_classif$cluster)))
colnames(result_NMI) <- c("Indice NMI", "Nombre de clusters")
write.csv2(x = result_NMI, file = paste(sortie, "Indice_NMI.csv", sep="/"))





################################## STEP 4.7 : CLASIF NON SUPERVISEE : Resultats-----

### Tableau de corespondance cluster et especes
tab_cor <- table(tab_classif$Espc_lt, tab_classif$cluster)

num_cluster <- as.numeric(colnames(tab_cor))

# Renomer les colonnes du tableau (Xn -> Cluster n)
vect_clus <- c()

for (i in 1:ncol(tab_cor)){
  vect_clus[i] <- paste("Cluster", i, sep = "-")
}

colnames(tab_cor) <- c(vect_clus)


write.csv2(tab_cor, file = paste(sortie, "Tableau_correspondance_cluster_esp.csv", sep = "/"))


### Nombre de cluster par espece 
tab_cor <- read.csv2(paste(sortie, "Tableau_correspondance_cluster_esp.csv", sep = "/")) # Relire fichier enregistrer pour avoir bon format

colnames(tab_cor) <- c("Espc_lt", vect_clus)

# Initialisation de la boucle
nb_cluster <- data.frame(1:nrow(tab_cor), 0)
nb_cluster <- nb_cluster[-1]
colnames(nb_cluster) <- c("Nb_cluster")

vect <- c() #vecteur compteur

for (i in 1:nrow(tab_cor)){
  
  for (j in 2:ncol(tab_cor)){
    if (tab_cor[i,j] != 0){
      
      vect[j+1] <- 1
       
    } else(vect[j+1] <- 0)
    
    nb_cluster[i, 1] <- sum(vect[3:length(vect)])
    
  }
  

}



tab_cor <- cbind(tab_cor, nb_cluster)

print(tab_cor)




### Compter le nombre de polygone du dataset par espece
tab_cor <- cbind(tab_cor, eff_classif) # variable eff_classif cree partie 4.6

print(tab_cor)




### Plot

# Nb de cluster d attribution par esp
graph1 <- ggplot(tab_cor, aes(x = Espc_lt, y = Nb_cluster)) +
  geom_bar(stat = "identity")

graph1 <-  graph1 + xlab("Espèces") + ylab("Nombres de cluster")

graph1 <- graph1 + scale_y_continuous(breaks = seq(0, max(tab_cor$Nb_cluster) + 1, 2))

graph1 <-  graph1 + coord_flip()




# Relation entre effectif de la classe et nombre de cluster
formule <- lm(tab_cor$Nb_cluster ~ tab_cor$eff_classif)
formule$coefficients
summary(formule)

graph2 <- ggplot(tab_cor, aes(x = eff_classif, y = Nb_cluster)) +
  geom_point(stat = "identity")

graph2 <-  graph2 + xlab("Nombre d'arbres echantillonnés") + ylab("Nombres de clusters")

graph2 <-  graph2 + geom_smooth(method = lm, se = FALSE)

graph2 <- graph2 + scale_x_continuous(breaks = seq(0, max(eff_classif) + 1, 2))


# Pourcentage de pixel par cluster et par espece et inversement, mise en forme de la base avant
tab_cor_melt <- tab_cor[ , 1:(length(vect_clus) + 1)] # Conserver que les colonnes cluster

sum_clus <- rowSums(tab_cor_melt[,-1])

for (i in 1:nrow(tab_cor_melt)){
  
  for (j in 2:length(vect_clus)){
    valeur <- (tab_cor_melt[i,j])/sum_clus[i]*100
    tab_cor_melt[i,j] <- valeur
  }
  
}
  

tab_cor_melt <- melt(tab_cor_melt)

vect_color <- primary.colors(length(unique(tab_cor_melt$Espc_lt)), steps = 6, no.white = TRUE) #creation palette de couleur


graph3 <- ggplot(tab_cor_melt, aes(x = Espc_lt, y = value, fill = Espc_lt)) +
  geom_bar(stat = "identity") +
  
  xlab("Espèces") + ylab("Pourcentage de pixel") +
  
  scale_fill_manual(values = vect_color) +
  
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  
  labs(fill = "Espèces") +
  
  guides(fill = guide_legend(reverse = TRUE)) +
  
  coord_flip() +
  
  facet_wrap(~ variable, ncol = 4) +
  
  theme(panel.grid.minor = element_blank(),           #removes minor grid lines
        strip.text = element_text(size = 7))               

# Recuperer la legende sur le plot pour la ploter a part
legend_graph3 <- cowplot::get_legend(graph3)

# Suprimer la legende une fois recuperer
graph3 <- graph3 + theme(legend.position = "none")




# Pourcentage de pixel par espece et par cluster
vect_color2 <- primary.colors(length(unique(tab_cor_melt$variable)), steps = 4, no.white = TRUE) #creation palette de couleur


graph4 <- ggplot(tab_cor_melt, aes(ordered(x = variable, levels = rev(levels(tab_cor_melt$variable))), y = value, fill = variable)) + 
  geom_bar(stat = "identity") + # infos : ordered() permet d inverser l axe des x en fonction des levels()
  
  xlab("Clusters") + ylab("Pourcentage de pixel") +
  
  scale_fill_manual(values = vect_color2) +
  
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  
  labs(fill = "Clusters") +
  
  coord_flip() +
  
  facet_wrap(~ Espc_lt, ncol = 4) +
  
  theme(panel.grid.minor = element_blank(),           #removes minor grid lines
        strip.text = element_text(size = 7))

# Recuperer la legende sur le plot pour la ploter a part
legend_graph4 <- cowplot::get_legend(graph4)

# Suprimer la legende une fois recuperer
graph4 <- graph4 + theme(legend.position = "none")

### Export
pdf(paste(sortie,"/", "Analyse_unsupervised_classification", ".pdf", sep = ""))

print(graph1)
print(graph2)

print(graph3)
grid.newpage()
grid.draw(legend_graph3)

print(graph4)
grid.newpage()
grid.draw(legend_graph4)


dev.off() 

