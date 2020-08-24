###################################################################################################################################################
#                                                                                                                                                 #
#                                 Etude de l'ecologie de la Roussette Noire (Pteropus niger) sur l Ile de La Reunion                              #
#                                                                                                                                                 #
#                                      -- Script de classification supervisee de la courronne des arbres  --                                      #
#                                                                                                                                                 #               
#                                                         ------ SCRIPT PARTIE 3.1 ------                                                         #
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

install.packages("e1071")


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

library(e1071) #autre package pour le SVM



################################## !! STEP 1.0 : Chemins creation du dataset------

# Chemins
dossier <- "D:/Donnees Terrain/Dalles" #Racine du chemin la ou sont situe tous les fichiers terrains .csv a fusionner

sortie <- "D:/Output_R_disque/Classification/Classification_supervisee"

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

# Exporter
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

image_multispectrale <- "D:/Pleiades/Traitement_Pansharpening/Zone_etude_fusionnee_balance_couleur.tif"

couche_ndvi <- "E:/Output_R/Zone_etude_fusionnee_NDVI.tif"

# Emprise de la nouvelle zone (EPSG:2975)
Xmin <- 355969
Ymin <- 7669857
Xmax <- 366724
Ymax <- 7678534

sortie_new_image <- "D:/Output_R_disque/Classification/Zone_reduite"
  



                     ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##


#### Pour l image Pleiade
raster_a_decouper <- stack(image_multispectrale)
raster_a_decouper_b1 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.1
raster_a_decouper_b2 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.2
raster_a_decouper_b3 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.3
raster_a_decouper_b4 <- raster_a_decouper$Zone_etude_fusionnee_balance_couleur.4

# Determiner l extension pour couper le raster
ext <- raster(xmn = Xmin, xmx = Xmax, ymn = Ymin, ymx = Ymax)

# Attribuer la projection voulu aux bandes de l image multispectrale
proj4string(raster_a_decouper_b1) <- CRS("+init=epsg:2975")
proj4string(raster_a_decouper_b2) <- CRS("+init=epsg:2975")
proj4string(raster_a_decouper_b3) <- CRS("+init=epsg:2975")
proj4string(raster_a_decouper_b4) <- CRS("+init=epsg:2975")

# Couper chaque bande en fonction de l extension definie
raster_crop_b1 <- crop(raster_a_decouper_b1, ext)
raster_crop_b2 <- crop(raster_a_decouper_b2, ext)
raster_crop_b3 <- crop(raster_a_decouper_b3, ext)
raster_crop_b4 <- crop(raster_a_decouper_b4, ext)

# Fusionner les bandes avec la nouvelles emprise
raster_crop_tot <- stack(raster_crop_b1, raster_crop_b2, raster_crop_b3, raster_crop_b4)

# Enregistrer le raster
writeRaster(x = raster_crop_tot, filename = paste(sortie_new_image,"Zone_etude_fusionnee_balance_couleur_extent_pour_classif.tif", sep = "/"))





#### Pour l indice NDVI
raster_a_decouper <- raster(couche_ndvi)

# Determiner l extension pour rogner le raster
ext <- raster(xmn = Xmin, xmx = Xmax, ymn = Ymin, ymx = Ymax)

# Attribuer la projection de notre raster a decouper au raster definissant l extension 
projection(ext) <- proj4string(ext) <- proj4string(raster_a_decouper)

# Couper le raster en fonction de l extension definie
raster_crop <- crop(raster_a_decouper, ext)

# Enregistrer le raster
writeRaster(x = raster_crop, filename = paste(sortie_new_image, "Zone_etude_fusionnee_NDVI_0.25_extent_pour_classif.tif", sep = "/"))









################################## !! STEP 3 : Calcule de la texture sur image Pleiades redimensionnee ------

# Chemins
image_multispectrale_classif <- "D:/Output_R_disque/Classification/Zone_reduite/Zone_etude_fusionnee_balance_couleur_extent_pour_classif.tif"

sortie_new_image <- "D:/Output_R_disque/Classification/Zone_reduite"




                    ## ## ## ## ## ## ## ## ## ## ## ## ## ##



# Import de la couche proche-infrarouge pour le calcule de la texture
img_mltiSpect <- stack(image_multispectrale_classif)
proj4string(img_mltiSpect) <- CRS("+init=epsg:2975")
band_nir <- img_mltiSpect$Zone_etude_fusionnee_balance_couleur_extent_pour_classif.4

# Calcul de la texture
T1 <- Sys.time()

indice_texture <- glcm(band_nir,
                       window = c(5,5),
                       statistics = c("mean", "variance", "homogeneity", "contrast","entropy"))

names(indice_texture) <- c("mean", "variance", "homogeneity", "contrast","entropy") #Renomer les colonnes dans le stack object


# Exporter les indices de textures en stack object
writeRaster(x = indice_texture, filename = paste(sortie_new_image,"Indices_texture.tif", sep = "/"))



T2 <- Sys.time()
Tdiff <-  difftime(T2, T1)
Tdiff
################################## !! STEP 4.0 : Chemin pour la classification ------

# Chemins

couche_ndvi <- "D:/Output_R_disque/Classification/Zone_reduite/Zone_etude_fusionnee_NDVI_0.25_extent_pour_classif.tif"

image_multispectrale <- "D:/Output_R_disque/Classification/Zone_reduite/Zone_etude_fusionnee_balance_couleur_extent_pour_classif.tif"

indices_texture <- "D:/Output_R_disque/Classification/Zone_reduite/Indices_texture.tif" # Stack oject avec tous les indices de texture

chemin_dataset <-"D:/Output_R_disque/Classification/Classification_supervisee" #Sans "/" a la fin. Dataset contenant les polygones d entrainement et de verification

buf <- 1 #Buffer en metres appliqué au points GPS pour extraire la signature spectrale des arbres

nom_fichier_dataset <- paste("Dataset_training_buffer", "_", buf, "m", sep ="") #Pas d extension (fichier shapefile)
# ATTENTION !! : dataset doit avoir en 3 premieres colonnes : X (Numerique = id), Espc_lat (nom latin esp), Espc_vr (nom vernaculaire esp)
# Apres ces trois premieres colonnes il doit y avoir les predicteurs

sortie <- "D:/Output_R_disque/Classification/Classification_supervisee"

nb_esp <- 1 # Effectif a partir du quel l espece est retenue dans la classification par le modele SVM


vars_non_corr <- c("NDVI", "Blue_band", "Red_band","Nir_band", 
                   "mean", "contrast", "entropy") 
# vecteur des noms de variables non correle conserve pour le modele SVM -> cf resultat matrice correlation
# Utiliser les noms bien orthographiés des predicteurs de la matrice de correlation




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







################################## STEP 4.2 : Import du traning dataset et assigner les valeurs des variables pour chaque polygone----
### Import du dataset avec polygones labelises par noms des especes
dataset_terrain <- readOGR(dsn = chemin_dataset, layer = nom_fichier_dataset)

# Verifications presence de projection et autres
summary(dataset_terrain)
# str(dataset)

### Creer la signature spectrale 
dataset_vars <- raster::extract(variables, dataset_terrain, fun = mean, sp = TRUE)


### Export
write.csv2(dataset_vars, file = paste(sortie, "Dataset_predicteurs.csv", sep = "/"))



################################## STEP 4.3 : Visualisation variables sur tout jeu de donnees d entrainement en fonction des especes arbres------

dataset_vars <- read.csv2(paste(sortie, "Dataset_predicteurs.csv", sep = "/"))


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
dataset_vars_freq <- data.frame(dataset_vars)
Effectif <- 1
dataset_vars_freq <- cbind(dataset_vars_freq, Effectif)

dataset_vars_freq <- aggregate.data.frame(dataset_vars_freq$Effectif, 
                                          by = list(dataset_vars_freq$Espc_lt), 
                                          FUN = sum, 
                                          na.rm = TRUE) # Sommer la colonne ajouter pour chaque variable

colnames(dataset_vars_freq) <-  c("Espc_lt", "Effectif")

graph11 <- ggplot(dataset_vars_freq, aes(x = Espc_lt, y = Effectif)) +
  geom_bar(stat = "identity")

graph11 <-  graph11 + xlab("Espèces") + ylab("Nombres de données")

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











################################## STEP 4.4 : Matrice de correlation ----

dataset_vars <- read.csv2(paste(sortie, "Dataset_predicteurs.csv", sep = "/"))


### Matrice de correlation

# Supprimer les 3 premieres colonnes (ID, esp latin et esp vernaculaire) pour garder que les variables 
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

                




################################## STEP 5.5 : Filtrer la base de donnees ----

dataset_vars <- read.csv2(paste(sortie, "Dataset_predicteurs.csv", sep = "/"))

### Garder uniquement esp latin 
dataset_vars <- dataset_vars[ , -1] # Enlever colone X (=ID)
dataset_vars <- dataset_vars[ , -2] # Enlever colonne nom vernaculaire




### Extraire les variables qui ne sont pas correlees du dataset
dataset_vars_non_cor <- dataset_vars[ ,1]

for (i in 1:length(vars_non_corr)){
  dataset_vars1 <- dplyr::select(dataset_vars, select = vars_non_corr[i])
  dataset_vars_non_cor <- cbind(dataset_vars_non_cor, dataset_vars1)
}

colnames(dataset_vars_non_cor) <- c("Espc_lt", vars_non_corr)
dataset_vars <- dataset_vars_non_cor
remove(dataset_vars_non_cor)




### Enlever les especes a faible effectif 
dataset_vars_freq <- data.frame(dataset_vars)
Effectif <- 1
dataset_vars_freq <- cbind(dataset_vars_freq, Effectif)

dataset_vars_freq <- aggregate.data.frame(dataset_vars_freq$Effectif, 
                                          by = list(dataset_vars_freq$Espc_lt), 
                                          FUN = sum, 
                                          na.rm = TRUE) # Sommer la colonne ajouter pour chaque variable

colnames(dataset_vars_freq) <-  c("Espc_lt", "Effectif")


# Extraire les esp a fort effectif sous forme de vecteur
esp_svm <- subset(dataset_vars_freq, Effectif >= nb_esp) 
esp_svm <- droplevels(esp_svm)
esp_svm <- esp_svm$Espc_lt
esp_svm <- levels(esp_svm)


# Conserver uniquement les especes a fort effectif, extraite ci-dessus, dans la BDD
dataset_svm <- data.frame()

for (i in 1:length(esp_svm)){
  dataset_svm1 <- subset(dataset_vars, Espc_lt == esp_svm[i])
  dataset_svm <- rbind(dataset_svm, dataset_svm1)
}


dataset_svm <- droplevels(dataset_svm) # Quand on filtre par des facteur on conserve l ensemble des facteurs 
                                       # du tabelau qui faut ensuite supprimer.
print(dataset_svm)




################################## STEP 5.6 : CLASIF SUPERVISEE : Separer le dataset en training et control pour validation croisee ------

### Preparation a la separation de la base
set.seed(27) # permettre la reproductibilite du code lorsque l on utilise l aleatoire
in_trainning <- createDataPartition(dataset_svm$Espc_lt, p = pourcentage, list = FALSE) #80% pour trainning et 20% pour verifier


### Creer deux BDD pour la classification et la verification
training <- dataset_svm[in_trainning, ]
testing <- dataset_svm[-in_trainning, ]


# Verification du jeu de donnee et du partitionnement
summary(training)
summary(testing)


################################## STEP 5.7 : CLASIF SUPERVISEE : Parametres le modele ------

### Parametrage de la validation croisee pour optimisation des parametres du modele
train_control<- trainControl(
  method = "repeatedcv",      # Methode de verification de l optimisation des parametre : cross-validation
  number = 5,                 # Separation du jeu de donnees en n parties
  repeats = 5)                # Repetition de la classification k fois



### Entrainement du modele

# Choisir le type de kernel en modifiant l indice kernel[?] : permet de recuperer l information sur le pdf d export
kernel <- c("svmLinear", "svmRadial", "svmPoly")
kernel <- kernel[1]

model_svm <- train(Espc_lt ~.,
                   data = training, 
                   method = kernel,                                               
                   trControl = train_control,  
                   preProcess = c("center","scale"),
                   tuneGrid = expand.grid(C = seq(from = 0, to = 10, by = 0.1)
                                          #sigma = seq(from = 0, to = 1, by = 0.1)
                                          #degree = seq(from = 1, to = 3, by = 1),
                                          #scale = seq(from = 1, to = 3, by = 1)
                                          )
                   )


### Parametres du modele utilises
print(model_svm)
svm_final <- model_svm$finalModel  # Recapitulation des parametres 
nb_pt_support <- svm_final@nSV     # Nombres de points supports
training_error <- svm_final@error
nb_obs_training <- nrow(training)  # Nombre d'observation d entrainement

result1 <- model_svm$bestTune      # Meilleurs parametres selon la validation croisee utilises dans le modele 
result1 <- cbind(result1, kernel)
result1 <- cbind(result1, nb_pt_support)
result1 <- cbind(result1, nb_obs_training)
result1 <- cbind(result1, training_error)
row.names(result1) <- "Parameter (CV)"

### Resultats validation croisee
result2 <- plot(model_svm) # Resultat de la validation croisee. Affichage impossible quand plus de 2 parametres testes


### Importance des variables
svm_varImp <- varImp(model_svm, compete = FALSE)
result3 <- plot(svm_varImp)



### Courbe ROC
# score <- predict(model_svm, newdata = testing[,-1], type = "prob")[ ,"yes"]
# 
# roc_obj <-roc(testing$Espc_lt == "yes", score)
# 
# print(roc_obj$auc)



### Matrice de confusion 

pred_svm <- predict(model_svm, newdata = testing[ ,-1])

result_matrix_conf <- confusionMatrix(data = pred_svm, testing$Espc_lt)

result4_matrix <- result_matrix_conf$table



# Export des parametres
pdf(paste(sortie,"/", "Resultats_modele_svm", ".pdf", sep = ""), height = 11, width = 8.5)
grid.table(result1)
print(result2)
print(result3)
dev.off() 

write.csv2(result4_matrix, paste(sortie, "Matrice de confusion.csv", sep = "/"))



################################## STEP 5.8 : CLASIF SUPERVISEE : Predictions du model sur les polygones de segmentation -----


### Extraire information des polygones issues de la segmentation et mettre la BDD en forme
seg <- readOGR()

BDD_a_predire <- raster::extract(variables, seg, fun = mean, sp = TRUE)



### Appliquer le modele sur la BDD
predict.svm(model_svm, BDD_a_predire["?":"?"])



