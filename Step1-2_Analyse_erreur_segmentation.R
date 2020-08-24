###################################################################################################################################################
#                                                                                                                                                 #
#                                 Etude de l'ecologie de la Roussette Noire (Pteropus niger) sur l Ile de La Reunion                              #
#                                                                                                                                                 #
#                                   -- Script de calcul de l erreur de segmentation de la courronne des arbres --                                 #
#                                                                                                                                                 #               
#                                                         ------ SCRIPT PARTIE 1.2 ------                                                         #
#                                                                                                                                                 #
#                                                           Romain FERNANDEZ - Mars 2020                                                          #
#                                                                                                                                                 #
###################################################################################################################################################


install.packages("raster")
install.packages("rgdal")


library(raster)
library(sp)
library(rgdal)
library(rgdal)
library(stars)
library(spatstat)
library(sf)

########################### STEP 0 : Reprojeter l image Pleiade en EPSG2975 si necessaire --------

image_multispectrale <- "D:/Pleiades/Traitement_Pansharpening/Zone_etude_fusionnee_balance_couleur.tif"

sortie_new_image <- "D:/Pleiades/Traitement_Pansharpening"


## ## ## ## ## ## ## ## ## ## ## ## ## ## ##


img_multi <- stack(image_multispectrale)

raster_a_decouper_b1 <- img_multi$Zone_etude_fusionnee_balance_couleur.1
raster_a_decouper_b2 <- img_multi$Zone_etude_fusionnee_balance_couleur.2
raster_a_decouper_b3 <- img_multi$Zone_etude_fusionnee_balance_couleur.3
raster_a_decouper_b4 <- img_multi$Zone_etude_fusionnee_balance_couleur.4

proj4string(raster_a_decouper_b1) <- CRS("+init=epsg:2975")
proj4string(raster_a_decouper_b2) <- CRS("+init=epsg:2975")
proj4string(raster_a_decouper_b3) <- CRS("+init=epsg:2975")
proj4string(raster_a_decouper_b4) <- CRS("+init=epsg:2975")

  
  
writeRaster(x = img_multi, filename = paste(sortie_new_image,"Zone_etude_fusionnee_balance_couleur_extent_pour_classif_EPSG2975.tif", sep = "/"))




########################### Chemins ------

dalles_seg <- "D:/Output_R_disque/Segmentation/Fusion" # Dossier avec fichiers shp (des polygones segmentes) issues du script segmentation 

nb_dalles_alea <- 70 # Nombre de dalle a selectionner sur la zone

nb_poly <-  80 # Nombre d arbre a detourer par dalle

sortie <- "D:/Output_R_disque/Segmentation/Calcul_erreur"

poly_a_del <- "Poly_reference_a_delimiter_40S"

poly_automatique <- "Poly_reference_delimite_40S"



# ############################ STEP 1 : Selection aleatoire des arbres a delimiter manuellement : polygones de reference ------------------
# 
# 
# ### Tirage aleatoire des dalles et des polygones
# 
# files <- list.files(path = dalles_seg, pattern = ".shp", recursive = TRUE, full.names = TRUE) # recuperer les nom des fichiers
# 
# # Choix nombre aleatoire dans les dalles
# set.seed(40)
# dalles_alea <- sample(1:length(files), nb_dalles_alea)
# 
# 
# # Ouverture des dalles et tirage de 10 polygones
# tab_alea <- read_sf(files[1])
# tab_alea <- tab_alea[1, ]
# 
# for (i in dalles_alea){
#   
#   print(i)
#   
#   tab <- read_sf(files[i])
#   poly_alea <- sample(1:nrow(tab), nb_poly)
#   
#     for (j in poly_alea){
#       print(j)
#       poly <- tab[j, ]
#       tab_alea <- rbind(tab_alea, poly)
#       remove(tab2)
#   }
#   
# }
# 
# tab_alea[-1 , ] # Supprimer la premiere ligne permettant la mise en forme du tableau
# 
# 
# ### Ajouter un identifiant unique a chaques polygones
# ID_tree <- c(1:nrow(tab_alea))
# tab_alea <- cbind(tab_alea, ID_tree) #Ajout de la colonne ID dans la table attributaire
# 
# 
# #### Supprimer l ancienne colonne ID (pas des ID unique)
# tab_alea <-dplyr::select(tab_alea, -treeID)
# 
# 
# 
# 
# # Export des polygone a delimiter en shapefile
# tab_alea1 <- as_Spatial(tab_alea)
# writeOGR(obj = tab_alea1,
#          dsn = sortie, 
#          layer = "Poly_reference_a_delimiter",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
# 
# 
# 
# 
# 
# ### Extraire en fichier shapefile les treetops des polygones extraits ci-avant (en conservant les attribus de la base saug geometrie polygones)
# 
# # Creer un SpatailPointDataFrame avec coordonnees treetop deja dans object sf en passant par un SpatailPolygonsDataFrame
# tab_alea <- as_Spatial(tab_alea) # Object sf to SpatailPolygonsDataFrame
# tab_alea <- data.frame(tab_alea) # SpatailPolygonsDataFrame to dataframe
# coordinates(tab_alea) <- ~X_treetop + Y_treetop # SpatailPolygonsDataFrame to SpatailPointDataFrame
# crs(tab_alea) <- "+proj=utm +zone=40 +south +ellps=GRS80 +units=m +no_defs"
# 
# # Export
# writeOGR(obj = tab_alea,
#          dsn = sortie, 
#          layer = "Treetop_reference_a_delimiter",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
# 
# 
# 

############################ STEP 2 : Calculer l erreur de la segmentation ------------------

############ STEP 2.1 : Mise en forme du tableau pour calculer les indices ------

##### Importer le shapefile

poly_auto <- read_sf(dsn = sortie, layer = poly_a_del) # En ESPG : 2975

poly_ref <- read_sf(dsn = sortie, layer = poly_automatique) # En ESPG : 2975
st_transform(x = poly_ref, crs = st_crs(poly_auto))

st_crs(poly_ref) <- st_crs(poly_auto) # Pour faire correspondre les DATUM et autre donnee externe a la projection mais pouvant empecher l intersection


##### Selectionner uniquement les polygones delimites
ID <- poly_ref$ID_tree

stockage1 <- c()

stockage2 <- poly_auto[1, ] # Recuperation de la forme et projection de l objet
stockage2 <- stockage2[-1, ]

for (i in 1:length(ID)){
  stockage1 <- subset(poly_auto, poly_auto$ID_tree == ID[i])
  stockage2 <- rbind(stockage2, stockage1)
}

poly_auto <- stockage2


###### Intersection des polygones automatiques et par delimitation manuelle
poly_auto <- poly_auto[order(poly_auto$ID_tree, decreasing = FALSE), ] # Mettre dans l ordre les deux tableaux pour cbind correct
poly_ref <- poly_ref[order(poly_ref$ID_tree, decreasing = FALSE), ]

poly_int <- st_intersection(poly_ref, poly_auto) # Selectionne automatiquement la colonne en commun aux deux tableaux


##### Calculer et extraire des aires des polygones

# Pour les polygones issues de la vectorisation manuel
tab1 <- poly_ref
poly_ref_area <- st_area(poly_ref)
tab1 <- cbind(tab1, poly_ref_area)
tab1


# Pour les polygones issues de la segmentation
tab2 <- poly_auto
poly_auto_area <- st_area(poly_auto) 
tab2 <- cbind(tab2, poly_auto_area)
tab2

# Pour les polygones issues de l intersection
tab3 <- poly_int
poly_int_area <- st_area(poly_int)
tab3 <- cbind(tab3, poly_int_area)
tab3

#Fusion des tableaux
tab_tot <- merge.data.frame(tab1, tab2, by = "ID_tree")
tab_tot <- cbind.data.frame(tab_tot, tab3)

tab_tot <- data.frame(tab_tot)


# Enlever les unite dans les variables du tableau (issu de l utilisation objet st) sinon blocage de la boucle ci-dessous
tab_tot$poly_ref_area <- as.numeric(tab_tot$poly_ref_area)
tab_tot$poly_auto_area <- as.numeric(tab_tot$poly_auto_area)
tab_tot$poly_int_area <- as.numeric(tab_tot$poly_int_area)



############ STEP 2.2 : Calculer l indice d erreur ------

# Initialisation des vecteurs de stockages de la boucle
over_seg = c()
under_seg = c()
coef_D = c()

# Boucle de calcule de l indice 
for (i in 1 :length(tab_tot$ID_tree)){
  over_seg1 <- 1 -  (tab_tot$poly_int_area[i] / tab_tot$poly_ref_area[i])
  over_seg[i] <- over_seg1
  
  under_seg1 <- 1 - (tab_tot$poly_int_area[i] / tab_tot$poly_auto_area[i])
  under_seg[i] <- under_seg1
  
  coef_D1 <- ((over_seg[i])^2 + (under_seg[i])^2) / 2
  coef_D[i] <- coef_D1
}

# Afficher les vecteurs de stockages de la boucle 
print(over_seg)
print(under_seg)
print(coef_D)

max(coef_D)

#### Boxplot : Analyse de D (Qualite de l ajustement) : D=0 : match parfait & D=1 : match minimum ------

boxplot_D <- boxplot(coef_D,
                     main = "Qualite d'ajustement de la segmentation automatique",
                     xlab = "Valeur de D",
                     ylab = NULL,
                     border = "blue",
                     horizontal =TRUE)


boxplot_over <- boxplot(over_seg,
                        main = "Mesure de la sur-segmentation du modele",
                        xlab = "Indice de sur-segmentation",
                        ylab = NULL,
                        border = "red",
                        horizontal =TRUE)

boxplot_under <- boxplot(under_seg,
                         main = "Mesure de la sous-segmentation du modele",
                         xlab = "Indice de sous-segmentation",
                         ylab = NULL,
                         border = "green",
                         horizontal =TRUE)


pdf(paste(sortie, "/", "Analyse_indice_D", ".pdf", sep = "")) # Ouvrir le PDF et enregistrer les sorties suivantes

# Boxplot multiple
boxplot(coef_D, over_seg, under_seg,
        main = "Mesure de l'erreur de la segmentation",
        at = c(1,2,3),
        col = topo.colors(3),
        las = 3,
        horizontal = TRUE,
        notch = FALSE)


# Ajouter la legende 
legend("bottomright", 
       inset = .00,
       title = "Analyse",
       legend = c("Coefficient D", 
                  "Indice sur-segmentation", 
                  "Indice sous-segmentation"), 
       fill = topo.colors(3), 
       horiz = FALSE, 
       cex = 0.8)



# dev.copy(device = PDF, 
#          filename = paste(sortie, "Summary_boxplot.pdf", sep = "/"), 
#          width = 500, 
#          height = 500) 

dev.off() 



#### Tableau resume ----

# Creer le tableau
tab_matrix <- matrix(nrow = 3, ncol = 6)
summary <- data.frame(tab_matrix)

# Renommer les lignes et les colones 
rownames(summary) <- c("Coefficient D", "Sur-segmentation", "Sous-segmentation")
names(summary) <- c("Minimum", "Q1", "Mediane", "Moyenne", "Q3", "Maximum")

# Ajouter les valeurs
summary[1,] <- summary(coef_D)
summary[2,] <- summary(over_seg)
summary[3,] <- summary(under_seg)


write.csv2(x = summary, 
           file = paste(sortie, "Summary_erreur.csv", sep = "/"))








############ STEP 2.3 : Analyses graphiques: Echantillonnage polygones manuels representatifs de la variation d aire des cimes ?------
#### Distribution de la frequences des airs des polygones delimites manuellement ------

pdf(paste(sortie, "/", "Frequence_airs_poly_auto_ref", ".pdf", sep = "")) # Imprimer les sorties dans un PDF


hist1 <- hist(x = tab_tot$poly_ref_area,
              freq = TRUE,
              main = "Frequences des aires des cimes d'arbres delimites manuellement",
              xlab = "Aire des cimes (m²)",
              ylab = "Nombre de polygones")



#### Distribution de la frequences des airs des polygones delimites automatiquement (Zaki et al. 2015) ----
hist2 <- hist(x = tab_tot$poly_auto_area,
              freq = TRUE,
              main = "Frequences des aires des cimes d'arbres delimites automatiquement",
              xlab = "Aire des cimes (m²)",
              ylab = "Nombre de polygones")



#### Distribution de la frequences de la difference des airs des polygones delimites manuellement et automatiquement ----
diff <- c()

for (i in 1:length(poly_ref_area)){
  diff[i] <- poly_ref_area[i] - poly_auto_area[i]
}

hist_diff <- hist(x = diff,
                  freq = TRUE,
                  main = "Frequences de la différence des aires des cimes d'arbres delimites automatiquement et manuellement",
                  xlab = "Différence d'aire (m²)",
                  ylab = "Fréquence")


dev.off() # fermer le PDF