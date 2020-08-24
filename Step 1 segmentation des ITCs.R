###################################################################################################################################################
#                                                                                                                                                 #
#                             Etude de l'ecologie de la Roussette Noire (Pteropus niger) sur l Ile de La Reunion                                  #
#                                                                                                                                                 #
#                               -- Script de segmentation de la courronne des arbres a l aide du MNT et MNS --                                    #
#                                                                                                                                                 #               
#                                                       ------ SCRIPT PARTIE 1.1 ------                                                           #
#                                                                                                                                                 #
#                                                        Romain FERNANDEZ - Mars 2020                                                             #
#                                                                                                                                                 #
###################################################################################################################################################


# AVERTISSEMENT !                                                                     
                                                                                                                                              
#  --> SCR du script en EPSG:2975 : Toutes les couches vecteurs et raster d'entree doivent respecter cette projection     

#  --> R version 3.6.1 (2019-07-05) utilisee pour la creation du script  

#  --> Installer Grass 7.6.1 dans "C:/Program Files/GRASS GIS 7.6" sinon modifier chemin dans code qui utilise le package RGrass7




# Library a installer -------
install.packages("sp")
install.packages("raster")
install.packages("rgdal")
install.packages("gdalUtils")
install.packages("ForestTools")
install.packages("RSAGA")
install.packages("rgeos")
install.packages("gdata")
install.packages("future.apply")
install.packages("parallel")
install.packages("stars")
install.packages("sf")
install.packages("tidyr")
install.packages("dplyr")
install.packages("rLiDAR")
install.packages("rgrass7")
install.packages("RStoolbox")
install.package("proj4")

# NON UTILISE :
install.packages("RSAGA")
install.packages("oceanmap")
install.packages("spatstat")
install.packages("spatialfil")
install.packages("magick")
install.packages("adimpro")
install.packages("OpenImageR")
install.packages("OpenImageR")


library(sp)
library(raster)
library(rgdal)
library(gdalUtils) 
library(ForestTools)
library(RSAGA)
library(rgeos)
library(PBSmapping)
library(gdata)
library(future.apply)
library(parallel)
library(stars)
library(sf)
library(tidyr)
library(dplyr)
library(rLiDAR)
library(rgrass7)
library(RStoolbox)
library(proj4)

# NON UTILISE :
library(RSAGA)
library(oceanmap) #laplacian filter
library(spatstat) #laplacian filter
library(spatialfil) #laplacian filter function edge
library(magick)
library(adimpro)
library(OpenImageR)
library(OpenImageR)


########################### Evaluer le temps du code --------

#install.packages("Benchmarking")
#library(Benchmarking)

#install.packages("profvis")
#library(profvis)
# Utiliser profvis::profvis({ }) pour visualiser le temps par etapes

# Package de base
# T1 <- Sys.time()
# T2 <- Sys.time()
# Tdiff <-  difftime(T2, T1)


############################ Enregistrer la position du script sur le PC en ouvrant avec le script R -----------

WD <- getwd()

# Parametrer un chemin vers un disque avec suffisament de memoire de stockage (Environ 300 Go)
Disque <- "E:/Output_R"





############################ STEP 1 (lancer avec projet R) : MNS : Fusion des tuiles pour formation zone d etude ----------

##### Positionner R dans le dossier ou sont les tuiles a fusionner 
setwd(WD)
setwd(paste(getwd(),"/Dalles_MNS", sep="")) 


##### Selectionner tous les noms des dalles en .tif dans le dossier
all_my_MNS <- list.files(path = getwd(), pattern = '\\.tif') # Selectionner toutes les noms des dalles dans le dossier


##### Fusion des tuiles en une seule
mosaic_rasters(gdalfile = all_my_MNS, 
               dst_dataset = paste(getwd(),"/Dalles_fusionnees/MNS_0.25m_Dalles_fusionnees.tif", sep=""), 
               of = "GTiff", 
               output_Raster = TRUE,
               overwrite = TRUE)

#Information sur la tuile totale creee (MNS_zone_etude.tif)
#gdalinfo("./Dalles_fusionnees/MNS_Dalles_fusionnees.tif") 




############################ STEP 2 (lancer avec projet R) : MNT : Fusion des tuiles pour formation zone d etude ---------- 

##### Positionner R dans le dossier ou sont stocke les tuiles MNT (fonction raster_mosaic tres sensible a ce positionnement)
setwd(WD)
setwd(paste(getwd(),"/Dalles_MNT", sep=""))

##### Conversion des tuiles .asc vers .tiff

# Selectionner tous les noms des dalles en .asc dans le dossier
vect_MNT_asc <- list.files(path = getwd(), pattern = "\\.asc$") 


# Application de la conversion des .asc en .tif sur toutes les dalles dans le dossier

# Initiation de la parallelisation
options(mc.cores = parallel::detectCores() - 1)
plan(multiprocess)
 

# Application a toutes les dalles en parallelisant le processus
future_lapply(vect_MNT_asc, function(x) {
  raster_MNT_asc <- raster(x) 
  proj4string(raster_MNT_asc) <- CRS("+init=epsg:2975") 
  newname <- paste(paste(getwd(),"/Dalles_MNT", sep=""), names = x)
  writeRaster(x = raster_MNT_asc,
              filename = newname, 
              bylayer = TRUE,
              format="GTiff",
              overwrite = TRUE) 
})



##### Fusionner les dalles re-encodees en .tiff precedement en une seule tuile
setwd(WD)
setwd(paste(getwd(),"/Dalles_MNT", sep=""))


##### Selectionner toutes les dalles en .tif raster dans le dossier
all_my_MNT <- list.files(path = getwd(), pattern = '\\.tif') # Selection des tuiles en tif


# Fusion des dalles en une seule
mosaic_rasters(gdalfile = all_my_MNT, 
               dst_dataset = paste(getwd(),"/Dalles_fusionnees/MNT_1m_Dalles_fusionnees.tif", sep=""), 
               of = "GTiff", 
               output_Raster = TRUE,
               overwrite = TRUE)

# Information sur la tuile totale creee (MNS_zone_etude.tif)
# gdalinfo(paste(getwd(),"/Dalles_fusionnees/MNT_Dalles_fusionnees.tif", sep="")) 


############################ STEP 3 (lancer avec projet R) : Decoupage de la zone selon un masque sur le MNS et le MNT -----
setwd(WD)



#### Pour le MNT
raster_a_decouper <- raster(paste(getwd(),"/Dalles_MNT/Dalles_fusionnees/MNT_1m_Dalles_fusionnees.tif", sep = ""))
#raster_a_decouper <- raster("E:/Output_R/MNT_disaggregate.tif")

# Determiner l extension pour rogner le raster
#ext <- raster(xmn=356218, xmx=356863, ymn=7678087, ymx=7679568) #extension des tests sur 2 dalles
ext <- raster(xmn=349049, xmx=373016, ymn=7664995, ymx=7690916) #extension zone etude totale

# attribuer la projection de notre raster a decouper au raster definissant l extension 
projection(ext) <- proj4string(ext) <- proj4string(raster_a_decouper)

# Couper le raster en fonction de l extension definie
raster_crop <- crop(raster_a_decouper, ext)

# Enregistrer le raster
writeRaster(x = raster_crop, filename = paste(getwd(),"/Dalles_MNT/Dalles_fusionnees/MNT_1m_Dalles_fusionnees_crop.tif", sep = ""))



                    ## ## ## ## ## ## ## ## ## ## ## ## ##


#### Pour le MNS
raster_a_decouper <- raster(paste(getwd(),"/Dalles_MNS/Dalles_fusionnees/MNS_0.25m_Dalles_fusionnees.tif", sep = ""))
#raster_a_decouper <- raster("E:/Output_R/MNT_disaggregate.tif")

# Determiner l extension pour rogner le raster
#ext <- raster(xmn=356218, xmx=356863, ymn=7678087, ymx=7679568) #extension des tests sur 2 dalles
ext <- raster(xmn=349049, xmx=373016, ymn=7664995, ymx=7690916) #extension zone etude totale

# attribuer la projection de notre raster a decouper au raster definissant l extension 
projection(ext) <- proj4string(ext) <- proj4string(raster_a_decouper)

# Couper le raster en fonction de l extension definie
raster_crop <- crop(raster_a_decouper, ext)

# Enregistrer le raster
writeRaster(x = raster_crop, filename = paste(getwd(),"/Dalles_MNS/Dalles_fusionnees/MNS_0.25m_Dalles_fusionnees_crop.tif", sep = ""))




############################ STEP 4 (lancer avec projet R) : Desaggreger le MNT (avec RGrass7 package)----------------
setwd(WD)

# Creation de l'environnement Grass
initGRASS("C:/Program Files/GRASS GIS 7.6",
          override = TRUE,
          gisDbase = "GRASS_TEMP",
          home = tempdir(),
          mapset = "PERMANENT",
          remove_GISRC = TRUE)

# modifier la projection de l'environnement
execGRASS("g.proj", flags = "c", epsg = 2975)

#Creer un nouveau mapset
#execGRASS("g.mapset", flags = "c", mapset = "new_mapset")

#Charger le MNT dans Grass
execGRASS("r.in.gdal",
          flags = c("overwrite"),
          parameters = list(input = paste(getwd(),"/Dalles_MNT/Dalles_fusionnees/MNT_1m_Dalles_fusionnees_crop.tif", sep = ""), 
                            output = "GRASS_MNT"))

execGRASS("r.in.gdal",
          flags = c("overwrite"),
          parameters = list(input = paste(getwd(),"/Dalles_MNS/Dalles_fusionnees/MNS_0.25m_Dalles_fusionnees_crop.tif", sep = ""), 
                            output = "GRASS_MNS"))

# Region : Pour que la sortie raster ait la mÃªme Ã©tendue et la mÃªme taille que le MNT
execGRASS("g.region", raster = "GRASS_MNS")
#execGRASS("g.region", res = "0.25")

gmeta()#resume de la region definie


#Desagreger le MNT
execGRASS(cmd = "r.resamp.interp",
          flag = "overwrite",
          parameters = list(input = "GRASS_MNT", output="MNT_desag_RGRASS", method = "bilinear"))

# Exporter le raster en .tif : si manque de place (Error 3) sur la zone de projet, changer le output vers un autre disque dur
execGRASS(cmd = "r.out.gdal",
          flag = "overwrite",
          parameters = list(input = "MNT_desag_RGRASS",
                            output = paste(Disque,"/MNT_1m_disaggregate_float32.tif", sep=""),
                            format = "GTiff"))


# Deconnecter l'environnement
unlink_.gislock() #supprimer le lien entre l'environnement crÃ©Ã© de GRASS et R
remove_GISRC() #supprimer le fichier permettant le positionnant de l'environnement dans "home"
tempdir() #permet de savoir ou se situe les tache d'arriÃ¨re plan de Grass





############################ STEP 5 (lancer avec projet R) : Creation du MNC ----
setwd(WD)

MNS_crop <- raster(paste(getwd(),"/Dalles_MNS/Dalles_fusionnees/MNS_0.25m_Dalles_fusionnees_crop.tif", sep = ""))

MNT_crop <- raster(paste(Disque,"/MNT_1m_disaggregate.tif", sep=""))

MNC <- MNS_crop - MNT_crop
#plot(MNC, main = "Modele Numerique de canopee (MNC)")


writeRaster(MNC, 
            filename = paste(getwd(), "/Output_data/MNC_0.25m_brut.tif", sep = ""), 
            format = "GTiff", 
            overwrite = TRUE)






############################ STEP 6 (lancer avec projet R) : Filtrer le MNC avec un filtre de convolution Gaussien (Processus long environ 6h pour la dalle utilisee) -------  

# Entrer des parametres du filtre et application du filtre 
setwd(WD)
chm <- raster("E:/Output_R/Test_Filter_Gauss/MNC_0.25m_brut_2_dalles.tif")

# Parametres du filtre
filter <- "Gaussian" # Type de filtre
ws <- 5              # Fenetre utilisee
sigma <- 2           # Valeur du sigma utilisee

# Application du filtre
MNC_filter_gauss <- rLiDAR::CHMsmoothing(chm, filter, ws, sigma)

# Exporter
writeRaster(MNC_filter_gauss, 
            filename = "E:/Output_R/Test_Filter_Gauss/MNC_0.25m_2_dalles_5x5.tif", 
            format = "GTiff", 
            overwrite = TRUE)

############################ STEP 7 : (A coder avec Grass - brouillon) Filtrer le MNC avec un filtre de convolution Laplacien Gaussien -------  
setwd(WD)
MNC <- raster(paste(getwd(), "/Output_data/MNC_filter_gaussian.tif", sep = ""))
MNC <- raster(paste(getwd(), "/Dalles_MNT/Dalles_fusionnees/MNT_Dalles_fusionnees.tif",  sep=""))

#### ######### Avec Grass
initGRASS("C:/Program Files/GRASS GIS 7.6",
          override = TRUE,
          gisDbase = "GRASS_TEMP",
          home = tempdir(),
          mapset = "PERMANENT",
          remove_GISRC = TRUE)

execGRASS(cmd = "r.resamp.filter",
          Flags = )

## ############################### #

MNC <- spatstat::as.array.im(MNC)
MNC <- oceanmap::raster2array(MNC)

MNC <- as.matrix(MNC)
OpenImageR::edge_detection(image = MNC, method = "LoG", conv_mode = "same", laplacian_type = 3 )

### ### ### ### ### ## ### ###
writeRaster(x = OpenImageR::edge_detection(image = MNC, method = "LoG", conv_mode = "same", laplacian_type = 3), 
            filename = paste(getwd(), "/Output_data/MNC_brut.tif", sep = ""), 
            format = "GTiff", 
            overwrite = TRUE)

## ############################### #
MNC <- spatstat::as.array.im(MNC)
MNC <- oceanmap::raster2array(MNC)

kernel <- spatialfil::convKernel(sigma = 3.75, k = "LoG")

MNC <- spatialfil::applyFilter(MNC, kernel = convKernel(sigma = 3.75, k = "LoG"))

## ############################### #

adimpro::make.image(MNC,compress=TRUE, gammatype="None", whitep = "D65",cspace="Adobe", scale="Original",xmode="RGB")

## ############################### #

spatstat::smooth(MNC, sigma = 3.75, kernel="gaussian", normalise=FALSE, bleed = TRUE, varcov=NULL)

## ############################### #

image <- MNC
magick::image_noise(MNC, noisetype = kernel_types("Laplacian"), sigma = 1)

############################ STEP 8.1 (lancer avec chemins): Creation de la couche NDVI (pour appliquer masques NDVI) -------------

# Chemins a modifier 

Image <- "D:/Pleiades/Traitement_Pansharpening/Zone_etude_fusionnee_balance_couleur.dat" # Chemin d entre de l image multispectrale fusionnee issue de Envi

sortie_img <- "E:/Output_R/" # Chemin de sortie de l image NDVI creee

MNCanope <- "F:/PTENIG_PY_R/PTENIG_R/Output_data/MNC_filter_gaussian.tif" # Modele numerique de canopee sur lequel va etre applique le filtre NDVI



                                         ## ## ## ## ## ## ## ## ## ## ## ## ##



#### Creation de la couche avec indice NDVI 

Pleiades <- stack(Image)

# renommer les bandes : blue (1), green (2), red (3), nir(4)
names(Pleiades) <- c("blue_band", "green_band", "red_band", "nir_band")

#### Creation de l indices NDVI
indice_veg <- spectralIndices(Pleiades,
                              red = 3,
                              green = 2,
                              blue = 1,
                              nir = 4,
                              indices = "NDVI")

projection(indice_veg) <- CRS("+init=epsg:2975")

writeRaster(indice_veg, 
            filename = paste(sortie_img, "Zone_etude_fusionnee_NDVI.tif", sep = ""), 
            format = "GTiff", 
            overwrite = TRUE)



                                          ## ## ## ## ## ## ## ## ## ## ## ## ##



### Desaggreger la couche NDVI pour quelle est la meme resolution que le MNC et que le masque puisse etre applique


# Creation de l'environnement Grass
initGRASS("C:/Program Files/GRASS GIS 7.6",
          override = TRUE,
          gisDbase = "GRASS_TEMP",
          home = tempdir(),
          mapset = "PERMANENT",
          remove_GISRC = TRUE)

# Modifier la projection de l'environnement
execGRASS("g.proj", flags = "c", epsg =2975)


# Charger la couche NDVI et MNC dans Grass
execGRASS("r.in.gdal",
          flags = "o",
          parameters = list(input = paste(sortie_img, "Zone_etude_fusionnee_NDVI.tif", sep = ""), 
                            output = "NDVI"))

execGRASS("r.in.gdal",
          flags = c("overwrite"),
          parameters = list(input = MNCanope, 
                            output = "MNC"))

# Region : Pour que la sortie raster ait la meme etendue et la meme taille que le MNT
execGRASS("g.region", raster = "MNC")

gmeta()#resume de la region definie


#Desagreger la couche NDVI
execGRASS(cmd = "r.resamp.interp",
          flag = "overwrite",
          parameters = list(input = "NDVI", output="NDVI_desag", method = "bilinear"))

# Exporter le raster en .tif : si manque de place (Error 3) sur la zone de projet, changer le output vers un autre disque dur
execGRASS(cmd = "r.out.gdal",
          flag = "overwrite",
          parameters = list(input = "NDVI_desag",
                            output = paste(sortie_img, "Zone_etude_fusionnee_NDVI_0.25_extent.tif", sep = ""),
                            format = "GTiff"))

# Deconnecter l'environnement (sinon stockage des donnees sur pc)
unlink_.gislock() #supprimer le lien entre l'environnement de GRASS et R
remove_GISRC() #supprimer le fichier permettant le positionnant de l'environnement dans "home"
tempdir() #permet de savoir ou se situe les fichiers/taches d'arriere plan de Grass



############################ STEP 8.2 :(lancer avec chemins): Creation du masque NDVI ------

NDVI <- "E:/Output_R/Zone_etude_fusionnee_NDVI_0.25_extent.tif" # Chemin de l image NDVI de la zone d etude

sortie_img <- "E:/Output_R/" # Chemin de sortie de l image NDVI creee

MNCanope <- "F:/PTENIG_PY_R/PTENIG_R/Output_data/MNC_filter_gaussian.tif" # Modele numerique de canopee sur lequel va etre applique le filtre NDVI


                      ## ## ## ## ## ## ## ## ## ## ## ## ##


#### Masquer les pixels sur le MNC ayant une valeur de NDVI qui montre que ce ne sont des arbres (NDVI < 0.65 )
MNC <- raster(MNCanope) #MNC a masquer

NDVI <- raster(NDVI) #couche masque NDVI desagregee

# Creer le masque
NDVI_mask <- NDVI < 0.65

writeRaster(NDVI_mask, filename = paste(sortie_img, "Mask_NDVI.tif", sep = ""), overwrite=TRUE)

############################ STEP 8.3 (lancer avec chemins) : Application du masque NDVI sur le MNC -------

# Remarque : Quitter R et re-ouvrir pour liberer la memoire tampon de Rstudio avant de lancer ce STEP et après avoir fait tourne le STEP 8.2


# Chemins
NDVI <- "E:/Output_R/Mask_NDVI.tif" # Chemin du masque NDVI cree

sortie_img <- "E:/Output_R/" # Chemin de sortie de l image NDVI creee

MNCanope <- "F:/PTENIG_PY_R/PTENIG_R/Output_data/MNC_filter_gaussian.tif" # Modele numerique de canopee sur lequel va etre applique le filtre NDVI



              ## ## ## ## ## ## ## ## ## ## ## ## ##

# Import donnees

MNC <- raster(MNCanope)

NDVI_mask <- raster(NDVI)

# Appliquer le masque
MNC_mask_NDVI <- mask(x = MNC, mask = NDVI_mask, maskvalue = TRUE)

# Exporter le resultat
writeRaster(MNC_mask_NDVI, filename = paste(sortie_img, "MNC_NDVI_mask.tif", sep = ""), overwrite=TRUE) #sortie du MNC masque




############################ STEP 9.1 (lancer avec chemins) : Creation du masques elevation sur le MNC. Enlever les pixels en dessous de 2m -------

# Chemins
MNC_mask <- "E:/Output_R/MNC_NDVI_mask.tif" #Chemin du MNC deja filtre par indice NDVI precedement

sortie_img <- "E:/Output_R/" # Chemin de sortie du MNC filtre par l elevation 



                          ## ## ## ## ## ## ## ## ## ## ## ## ##


# Import raster
MNC <- raster(MNC_mask)

# Creer le masque d elevation
elevation_mask <- MNC < 2

# Exporter le masque
writeRaster(elevation_mask, filename = paste(sortie_img, "Mask_elevation.tif", sep = ""), overwrite=TRUE)

############################ STEP 9.2 (lancer avec chemins) : Application du masques elevation sur le MNC -------

# Remarque : Quitter R et re-ouvrir pour liberer la memoire tampon de Rstudio avant de lancer ce STEP et après avoir fait tourne le STEP 9.1



# Chemins
MNC <- "E:/Output_R/MNC_NDVI_mask.tif" #Chemin du MNC filtre par NDVI precedement

sortie_img <- "E:/Output_R/" # Chemin de sortie du MNC filtre par l elevation 

elevation_mask <- "E:/Output_R/Mask_elevation.tif"



                         ## ## ## ## ## ## ## ## ## ## ## ## ##



# Charger les rasters
elevation_mask <- raster(elevation_mask)
MNC <-  raster(MNC)

# Application du masque
MNC_mask_elevation <- mask(x = MNC, mask = elevation_mask, maskvalue = TRUE)

writeRaster(MNC_mask_elevation, filename = paste(sortie_img, "MNC_NDVI_elevation_mask.tif", sep = ""), overwrite=TRUE)



############################ STEP 10.1 (lancer avec chemins) : Segmentation selon watershed ------
# Chemins

  
MNC_final <- "E:/Output_R/MNC_NDVI_elevation_mask.tif" # chemin du MNC final

sortie_seg <- "D:/Output_R_disque/Segmentation" # Sortie de la couche shp treetop



                    ## ## ## ## ## ## ## ## ## ## ## ## ##




# Chargement du MNC
MNC_Filter <- raster(MNC_final) 


##### Extraction des sommets des arbres pour qualibrer la segmentation : Creation de la fenetre de recherche

# Creation d une fenetre adaptative (window filter algorithm by Popescu and Wynne (2004)). Taille de la fenetre change 
# en fonction de la valeur de hauteur du pixel centre (x) ?
window_research <- function(x){x * 0.05 + 0.6} #3.5 a la place de 0.6 ?



#### Extraction de la cime des arbres en utilisant la fenetre creee : Boucle traitant des dalle de 500m x 500m avec un buffer de 20m

# Creer des vecteurs de decoupe
x_origin <- MNC_Filter@extent@xmin
y_origin <- MNC_Filter@extent@ymin

x_max <- MNC_Filter@extent@xmax
y_max <- MNC_Filter@extent@ymax

x.seq <- seq(from = x_origin + 20, to = x_max, by = 500)
y.seq <- seq(from = y_origin + 20, to = y_max, by = 500)



# Boucle : Creer plusieurs masque séparant le MNC en plusieurs dalles qui sont ensuite segmentees. 
# Les coordonnees gps des treetops sont aussi ajoutes a la table attributaire de la couche shpefile de delimitation des houppiers 

length_X <- length(x.seq)-1 #-1 car calcule des dalles entre x1 et x+1
length_Y <- length(y.seq)-1 #-1 car calcule des dalles entre y1 et y+1

for(i in 32:length_X){ 
  for(j in 1:length_Y){
    
    print(paste("Colonne :", i))
    print(paste("Ligne :", j))
  
    
    # Division de la zone en plusieurs dalles de 500m (540m exactement)
    print("Division de la zone en dalles")
    mask_dalle_MNC <- raster(xmn = x.seq[i] - 20, xmx = x.seq[i+1] + 20, 
                           ymn = y.seq[j] - 20, ymx = y.seq[j+1] + 20)
  
    res(mask_dalle_MNC) <- 0.25
    proj4string(mask_dalle_MNC) <- CRS("+init=epsg:2975")
  
    dalle <- crop(MNC_Filter, mask_dalle_MNC)
    
    
    
    # Extraction des sommets des houppiers
    print("Extraction des treetops")
    try(treetop <- vwf(CHM = dalle, 
                   winFun = window_research,    # Fenetre de recherche de la cime des arbres
                   minHeight = 2,               # Pixels en dessous de 2m ne peuvent pas etre des cimes
                   verbose = FALSE))             # Affichage dans la console de l avancement
    
    
    if(exists("treetop") == TRUE){
      
      print("Presence de treetops")
      
      # Delimitation des houppiers a l aide des treetops
      print("Delimitation des houppier")
      crownsPoly <- mcws(treetops = treetop, 
                         CHM = dalle, 
                         format = "raster",       # "raster" ou "polygons" --> polygone est beaucoup plus long, conversion ensuite plus rapide (Stars object)
                         minHeight = 1,           # (3.5m < 4m) La valeur de l argument minHeight de la fonction mcws doit avoir une valeur plus basse que celle assignee a vwf.
                         verbose = FALSE)         # Affichage dans la console de l avancement
      
      
      
      # Conversion du fichier raster de sortie de la fonction mcws en fichier shapefile
      print("Conversion du raster en fichier shapefile")
      r <- stars::st_as_stars(crownsPoly)               #import raster issu de la segmente comme objet star 
      
      crownsPoly <- sf::st_as_sf(r, merge=TRUE)         # transformation star object to sf object 
      
      crownsPoly <- sf::as_Spatial(crownsPoly)
      
      crs(crownsPoly) <- "+init=epsg:2975 +proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
      
      
      # Extraction des coordonnees de tous les treetop trouves et les ajouter a la table attributaire
      print("Jointure des coordonnees des treetops et des houppiers")
      coord_treetop <- coordinates(treetop)      # Extraction des coordonnees
      colnames(coord_treetop) <- c("X_treetop", "Y_treetop")
      treetop <- cbind(treetop, coord_treetop)   # Ajouter a la table attributaire
      
      
      # Converstion des objects star en objects sf pour pouvoir faire la jointure
      crownsPoly <- st_as_sf(crownsPoly)
      treetop <- st_as_sf(treetop)
      
      #Jointure des tables attributaires points et polygones par la localisation
      jointure <- st_join(crownsPoly, treetop)
      
  
      # Eliminer les houppiers qui ont leur treetop dans la zone tampon autour des dalles (bordures)
         BDD <- jointure %>%
           filter(X_treetop >= x.seq[i]) %>%
           filter(X_treetop <= x.seq[i+1]) %>%
           filter(Y_treetop >= y.seq[j]) %>%
           filter(Y_treetop <= y.seq[j+1])
  
      # verifier que la dalle n est pas vide avant enregistrement (que le filtre des bordure n a pas elimine tous les arbres)
         if (is.na(BDD[1,5]) == TRUE){ #Regarder si la colonne IDtree de la premiere ligne est vide, si oui --> pas de donnees
           
           print("Pas d enregistrement car dalle sans treetop apres filtrage des bordures")
           
           print("Objet treetop supprime")
           remove(treetop)
           
         } else {
           
           # Exporter la couche shapefile des houppiers
           print("Enregistrement de la dalle vectorisee et filtree")
           BDD <- as_Spatial(BDD)
           writeOGR(obj = BDD,
                    dsn = sortie_seg, layer = paste("Dalle_segmentation_X_", x.seq[i], "_Y_", y.seq[j], sep=""),
                    driver = "ESRI Shapefile",
                    overwrite = TRUE)
          
           
           # Supprimer l object treetop cree a chaque fois qu il est cree durant une iteration pour que le test try n utilise pas l object de l iteration precedente
           print("Objet treetop supprime")
           remove(treetop)
         }
         
      
      
      
    } else(print("Dalles ignoree car ne contient pas de treetops"))
    
    j <- j+1
  } #fin boucle j
  
  i <- i+1
} #fin boucle i







############################ STEP 10.2 (lancer avec chemins) : Segmentation selon watershed : Fusion des dalles segmentees------


sortie_seg <- "D:/Output_R_disque/Segmentation" # Chemin de sortie des dalles segmente a fusionner en plus grosse zone

sortie_fusion <- "D:/Output_R_disque/Segmentation/Fusion" # Chemin de sortie des plus grosses zones

MNC_final <- "E:/Output_R/MNC_NDVI_elevation_mask.tif" # chemin du MNC final


#### Creation a nouveau des meme vecteurs x.seq et y.seq que ceux cree pour la segmentation des dalles au STEP 10.1
# Ces vecteurs sont utilises pour fusionner les dalles decoupe grace a ce vecteur au STEP 10.1
MNC_Filter <- raster(MNC_final) 

# Creer des vecteurs de decoupe
x_origin <- MNC_Filter@extent@xmin
y_origin <- MNC_Filter@extent@ymin

x_max <- MNC_Filter@extent@xmax
y_max <- MNC_Filter@extent@ymax

x.seq <- seq(from = x_origin + 20, to = x_max, by = 500)
y.seq <- seq(from = y_origin + 20, to = y_max, by = 500)



### Creation d un SpatialPolygonDataFrame vide, a remplir le long de la boucle
merg_model <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[1], "_Y_", y.seq[1], ".shp", sep=""))

merg_initialisation <- SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame())
crs(merg_initialisation) <- crs(merg_model)



#### Merger toutes les dalles shapefiles segmentees en plusieurs zones
file_name <- list.files(sortie_seg, pattern = "shp")

  
for (i in seq(from = 1, to = length(x.seq), by = 5)){
  
  for (j in seq(from = 1, to = length(y.seq), by = 5)){
    
    print("Initialisation")
    shapefile_finale <- merg_initialisation
    
    print(paste("Colonne de depart :", i))
    print(paste("Ligne de depart :", j))
    
    
    
    ############  Pour x de 1 a 5 et j = 1
    print("calcule de x à x+5 et j=1")
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i], "_Y_", y.seq[j], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+1], "_Y_", y.seq[j], ".shp", sep="")))
      if(exists("merg") == FALSE){
        
        print("Dalle ignoree")
        
      } else {
        shapefile_finale <- bind(shapefile_finale, merg)
        remove(merg)
      }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+2], "_Y_", y.seq[j], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+3], "_Y_", y.seq[j], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+4], "_Y_", y.seq[j], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    
    
    
    ############ Pour x de 1 a 5 et j = 2
    print("calcule de x à x+5 et j=2")
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i], "_Y_", y.seq[j+1], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+1], "_Y_", y.seq[j+1], ".shp", sep="")), silent = TRUE)
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+2], "_Y_", y.seq[j+1], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+3], "_Y_", y.seq[j+1], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+4], "_Y_", y.seq[j+1], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
  
    
    ############ Pour x de 1 a 5 et j = 3
    print("calcule de x à x+5 et j=3")
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i], "_Y_", y.seq[j+2], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+1], "_Y_", y.seq[j+2], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+2], "_Y_", y.seq[j+2], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+3], "_Y_", y.seq[j+2], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+4], "_Y_", y.seq[j+2], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    
    
    ############ Pour x de 1 a 5 et j = 4
    print("calcule de x à x+5 et j=4")
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i], "_Y_", y.seq[j+3], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+1], "_Y_", y.seq[j+3], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+2], "_Y_", y.seq[j+3], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+3], "_Y_", y.seq[j+3], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+4], "_Y_", y.seq[j+3], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    
    
    
    ############ Pour x de 1 a 5 et j = 5
    print("calcule de x à x+5 et j=5")
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i], "_Y_", y.seq[j+4], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+1], "_Y_", y.seq[j+4], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+2], "_Y_", y.seq[j+4], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+3], "_Y_", y.seq[j+4], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    try(merg <-readOGR(paste(sortie_seg, "/Dalle_segmentation_X_", x.seq[i+4], "_Y_", y.seq[j+4], ".shp", sep="")))
    if(exists("merg") == FALSE){
      
      print("Dalle ignoree")
      
    } else {
      shapefile_finale <- bind(shapefile_finale, merg)
      remove(merg)
    }
    
    
    # Enregistrement de la dalle mais tester si l object de stockage a bien de donnees
    
    try(nrow(shapefile_finale))
    
    if(nrow(shapefile_finale) != 0){
      print("Enregistrement de la dalle finale")
      writeOGR(obj = shapefile_finale,
             dsn = sortie_fusion,
             layer = paste("Dalle_fusionne_X_", x.seq[i], "_Y_", y.seq[j], sep = ""),
             driver = "ESRI Shapefile",
             overwrite = TRUE)
      
      rm(shapefile_finale)
      gc()
      
    } else{
      print("Pas d enregistrement car dalle vide")
      rm(shapefile_finale)
      gc()
    }
      
      
    
    rm(shapefile_finale)
    gc()
  
    
  } # fin boucle j
  
} # fin boucle i






############################ (PAS UTILISE) STEP 11 : Segmentation selon watershed : Mise en forme du tableau attributaire de la couche de segmentation----

# Chemins

segmentation_finale <- "D:/Output_R_disque/Segmentation/Segmentation_Zone_etude_finale.shp"

sortie_seg <- "D:/Output_R_disque/Segmentation"




                ## ## ## ## ## ## ## ## ## ## ##




# Importer les fichier shp en objet star, ajouter d'une colone ID unique
poly_auto <- st_read(segmentation_finale)  


#### Supprimer polygone sans treetop ou petite couronne (artefacts)
poly_auto <- filter(poly_auto, treeID != 0)


#### Ajouter un ID unique a chaque ITC 
ID_tree <- c(1:nrow(poly_auto))
poly_auto <- cbind(poly_auto, ID_tree) #Ajout de la colonne ID dans la table attributaire


#### Supprimer l ancienne colonne ID (pas des ID unique)
poly_auto <-dplyr::select(poly_auto, -treeID)


#### Exportation
poly_auto_conv <- as_Spatial(poly_auto) # Repasser en shapefile pour exporter
writeOGR(obj = poly_auto_conv, dsn = sortie_seg, layer = "Segmentation_attribu_filter_treetop", driver = "ESRI Shapefile", overwrite = TRUE)


          ## ## ## ## ## ## ##


#### Calculer l air de chaque polygone
area <- area.poly(poly_auto)
poly_auto <- cbind(poly_auto, area)


# Supprimer les ITC < 5m²                             - de 5m² ?? ## ??##?? ## ??## ??##?? ##?? ## ??## ??## ??##?? ##?? ## ??##?? ##
poly_auto <- filter(poly_auto, area > 5)

# Exportation
poly_auto <- as_Spatial(poly_auto) # Repasser en shapefile pour exporter

#### Exportation
writeOGR(obj = poly_auto, dsn = sortie_seg, layer = "Segmentation_attributs_filter_area", driver = "ESRI Shapefile", overwrite = TRUE)


############################ (PAS UTILISE) STEP 12 : Segmentation selon watershed : Verification rapide ------


# Chemins
segmentation1 <- "D:/Output_R_disque/Segmentation/Segmentation_attribu_filter_treetop.shp" #Segmentation filtrer par l absence d ID 
segmentation2 <- "D:/Output_R_disque/Segmentation/Segmentation_attributs_filter_area.shp"  #Segmentation filtrer par l aire des houppiers

sortie_seg <- "D:/Output_R_disque/Segmentation"




                      ## ## ## ## ## ## ## ## ## ## ## ## ## ##




#### Verification rapide de la segmentation 

# Import des objets necessaires
setwd(WD)
segmentation_attributs_1 <- readOGR(segmentation1)
segmentation_attributs_2 <- readOGR(segmentation2)

#Analyses
area <- summary(segmentation_attributs_1 $area)
area2 <- summary(segmentation_attributs_2 $area)
hauteur <- summary(segmentation_attributs_1 $height)

#Pourcentage de donnee filtre par la taille de la couronne 
segmentation_attributs_1 <- data.frame(segmentation_attributs_1)
segmentation_attributs_2 <- data.frame(segmentation_attributs_2)
nb1 <- nrow(segmentation_attributs_1)
nb2 <- nrow(segmentation_attributs_2)

pourcentage <- 100 - (nb2 * 100) / nb1

setwd(WD)
write.csv2(x = pourcentage, file = paste(sortie_seg, "Summary_rapide_pourcentage_de_donnees_filtrees.csv", sep = "/"))


# Creation du tableau
tab_tot <- rbind(area, area2)
tab_tot <- rbind(tab_tot, hauteur)
row.names(tab_tot) <-  c("Aires des polygones non filtre par la surface", "Aires des polygones filtre par la surface", "Hauteurs des cimes")
colnames(tab_tot) <- c("Minimum", "Q1", "Mediane", "Moyenne", "Q3", "Maximum")


setwd(WD)
write.csv2(x = tab_tot, file = "./Output_data/Summary_rapide.csv")
