
#### Determination Of Cropclasses
#### RapidEye - Project Environmental Sciences
#### University of Koblenz-Landau



#Installing required packages
#install.packages("pacman")
#pacman::p_load(raster, mapview, rasterVis, devtools)
#install.packages("backports")
#install.packages("ModelMetrics")
#install.packages("caret")
#devtools::install_github("krlmlr/here")
#devtools::install_github("pat-s/hsdar")

#loading required packages
library(raster)
library(rasterVis)
library(mapview)
library(hsdar)
library(here)

#-------------- Input -----------------

#set wd
datadir <- "C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidEye"
setwd(datadir)

file <- list.files(file.path(datadir, "3362507_2012-07-24_RE2_3A_326529"), pattern = ".tif", full.names = TRUE)

file

#read the files in R, nlayers shows the number of bands
rapid <- raster::brick(file[1])
rapid

#-------------- Visualization ------------

#quick look at the image, number from 1 to 5 can be chosen (5 bands sample)
rasterVis::levelplot(rapid[[1]], margin = FALSE, pretty = TRUE)
mapview(rapid[[1]], na.color = "transparent", map.types = "Esri.WorldImagery")


#-----------Transforming the Rasterclass so hsdar can use it------------

#apply the wavelengthborders from metadata
#Vector with corresponding wavelength for each band

#get.sensor.characteristics optional

wavelength <- c(440, 520, 630, 690, 760)
wavelength

#change format
rapidclass <- hsdar::HyperSpecRaster(rapid, wavelength)
class(rapidclass)

#optional speclip for further steps
#rapidclass <- hsdar::speclib(rapid, wavelength)
#class(rapidclass)


#-------------------- Vegetation Indices --------------

#required packages
#devtools::install_github("pat-s/rasterFunctions")
#install.packages("colorRamps")
#install.packages("tiff")
library(rasterFunctions)
library(colorRamps)


#NDVI calculation 
time <- Sys.time()
ndvi_hyperspec <- vegindex(rapidclass, index = "NDVI3", progress = 'text' )
Sys.time() - time

#quick look
rasterVis::levelplot(ndvi_hyperspec, margin = FALSE,
                     pretty = TRUE, col.regions = rev(colorRamps::green2red(400)), 
                     xlab = "Coordinates x", ylab = "Coordinates y", main = "NDVI RapidEye")


#export data
writeRaster(ndvi_hyperspec, "3362507_2012-07-24_RE2_3A_326529_NDVI2", format = "GTiff", overwrite=TRUE)

#calculate NDVI

ndvifunc <- function(img, i, k) {
  bi <- img[[i]]
  bk <- img[[k]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

#for RapidEye 3, 4 #NIR, Red
ndvi_man <- ndvifunc(rapidclass, 4, 3)
plot(ndvi, col = rev(terrain.colors(30)), main = 'NDVI from RapidEye', xlab = "Coordinates x", ylab = "Coordinates y")


#export data
writeRaster(ndvi_man, "3362507_2012-07-24_RE2_3A_326529_NDVIman1", format = "GTiff", overwrite=TRUE)

#================= OPTIONAL: Principal Component Analysis ===============
#Was not used for our 5 bands data


set.seed(1)
sr <- sampleRandom(rapid, 10000)
plot(sr[,c(2,4)])

pca <- prcomp(sr, scale = TRUE)
pca
plot(pca)

pci <- predict(rapid, pca, index = 1:2)
spplot(pci, col.regions = rev(heat.colors(20)), cex=1,
       main = list(label="First 2 principal components from Rapideye"))
plot(pci)

#====================== Unsupervised Classification =============

#-------Option 1 (source: "Remotesensing and GIS for Ecologists- Martin Wegmann")------


#install.packages("RStoolbox")
#required packages
library(RStoolbox)

set.seed(6)
UC_2012_1 <- unsuperClass(ndvi_hyperspec, nClasses = 5, nStarts = 50, nSamples = 10000, norm = TRUE)

#plot the ndvi
plot(UC_2012_1$map)

#export data
writeRaster(UC_2012_1$map, "3362507_2012-07-24_RE2_3A_326529_UC1_1", format = "GTiff", overwrite=TRUE)

#--------Option 2(source: "R Spatial Tutorial- rspatial.org")-----------------

gc() #clear RAM
km <- kmeans(values(ndvi_hyperspec), centers=5, iter.max=500,
             nstart=3, algorithm="Lloyd")     #high memory use

plot(km$cluster)
UC_2012_2 <- setValues(ndvi_hyperspec, km$cluster)

#plot the ndvi
plot(UC_2012_2, main = 'Unsupervised classification of RapidEye')

#export data
writeRaster(UC_2012_2, "3362507_2012-07-24_RE2_3A_326529_UC2_2", format = "GTiff", overwrite=TRUE)


#Optional: mask using atkis layer
UC_2012_2_mask <- mask(UC_2012_2, atkis_4101)

#==================== editing LUCAS data  =============

#required packages
library(dplyr)
library(utils)
library(tidyr)
library(sf)
library(RPostgreSQL)
library(postGIStools)
library(DBI)
library(sp)
library(rgeos)
library(rgdal)


#set wd
wd <- ("C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften")
setwd(wd)

#------------ get the LUCAS/Atkis Data from our server
prj = "C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidProject" # Enter path to DB_access.R here!
RSdatadir = 'C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidEye'
source(file.path(prj, "src", "DB_access_ken.R"))

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = DBname_1, user = DBuser, host = DBhost, port = DBport, password = DBpassword)

ger_states = st_read_db(con, table = c("spatial_derived", "germany_bld"))
ger_states = ger_states[ger_states$gen == 'Sachsen', ]

ezgs = st_read_db(con, query = "SELECT * 
                  FROM spatial_derived.land_use_ezg30
                  WHERE substring(site_id, 1, 2) = 'SN'")

atkis_4101 = st_read_db(con, query = "SELECT *
                        FROM \"atkis_DLM\".veg01_f_dtl_31467
                        WHERE oba = '4101' AND land = 'sn';")
luc12_SN_4101 = st_read_db(con, query = 
                             "SELECT *
                           FROM spatial_derived.lucas12") 

dbDisconnect(con)
dbUnloadDriver(drv)
#---------------------

#view table
View(luc12_SN_4101)

#organize column and add group column

##         class   Hektar Prozent
##     Ackerland 11846400    98.8
##      Getreide  6180300    51.5
##          Mais  2555900    21.3
##          Raps  1285500    10.7
##  Zuckerrueben   312800     2.6
##     Grasanbau   267800     2.2
##   Leguminosen   258400     2.2
##    Kartoffeln   236700     2.0
##          Wein   102544     0.9
##        Erbsen    79100     0.7
##       Obstbau    44744     0.42


luc12_SN_4101$lc1_group <- ifelse(grepl('^E',luc12_SN_4101$lc1), "Grasanbau", 
                                             ifelse(grepl('B55',luc12_SN_4101$lc1), "Grasanbau",
                                                    ifelse(grepl('B16',luc12_SN_4101$lc1), "Mais", 
                                                           ifelse(grepl('^B1',luc12_SN_4101$lc1), "Getreide",
                                                                  ifelse(grepl('B32',luc12_SN_4101$lc1), "Raps", 
                                                                         ifelse(grepl('B22',luc12_SN_4101$lc1), "Zuckerrüben", 
                                                                                ifelse(grepl('B50',luc12_SN_4101$lc1), "Leguminosen", 
                                                                                       ifelse(grepl('B21',luc12_SN_4101$lc1), "Kartoffeln", 
                                                                                              ifelse(grepl('B82',luc12_SN_4101$lc1), "Wein",
                                                                                                     ifelse(grepl('B41',luc12_SN_4101$lc1), "Erbsen", 
                                                                                                            ifelse(grepl('B70',luc12_SN_4101$lc1), "Obst", NA)))))))))))
#view table
View(luc12_SN_4101)

#count groups
count(luc12_SN_4101, lc1_group)

#optional: write table
write.table(luc12_SN_4101, "C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidProject/LucasGroups.txt")

#unload package that causes errors in further steps
detach(package:tidyr)

class(luc12_SN_4101)

#========== Prepare LUCAS points ===============

#require packages
library(rpart)
library(raster)
library(mapview)

#bounding box raster
bbox <- extent(rapid)
bbox <- as(bbox, 'SpatialPolygons') # convert to sp:: object
bbox <- st_as_sf(SpatialPolygonsDataFrame(bbox, data.frame(ID = 1))) #! data.frame()

#add CRS information
st_crs(bbox)$proj4string <- raster::projection(rapid) 
bbox <- st_transform(bbox, crs = 31467) # transform to 31467

st_crs(rapid)$proj4string

#CRS change and crop
luc12_SF <- st_transform(luc12_SN_4101, st_crs(bbox)$proj4string)
luc_crop <- st_contains(bbox, luc12_SF)
luc_crop
luc12_SF_bbox <- luc12_SF[unlist(luc_crop),]

rapid
class(luc12_SF_bbox)
#-> different CRS

#CRS change
luc12_SF_bbox <- st_transform(luc12_SF_bbox, crs = projection(rapid))

#from sf to sp
luc12_SP <- as(luc12_SF_bbox, "Spatial")

#data info
class(luc12_SP)
View(luc12_SP)

# extract values with points
luc12_SP_Ex <- raster::extract(rapid, luc12_SP)

class(luc12_SP)
class(rapid)

#quick look
mapview(rapid) +
  mapview(luc12_SP)

#========= Bands for Sprectral Profile ==============
#------>creates a spectral profile plot

ms <- aggregate(luc12_SP_Ex, list(luc12_SP$lc1_group), mean)
head(ms)
rownames(ms) <- ms[,1]
ms <- ms[,-1]
ms

mycolor <- c('cyan', 'darkgreen', 'yellow', 'burlywood',
             'darkred', 'darkgray', 'blue', 'lightgreen')

#transform ms from data.frame to matrix
ms <- as.matrix(ms)

#first create empty plot
plot(0, ylim=c(100, 10000), xlim = c(1,5), type='n', xlab="Bands", ylab = "Reflectance")

#add the different classes
for (i in 1:nrow(ms)){
  lines(ms[i,], type = "l", lwd = 3, lty = 1, col = mycolor[i])
}

#set title
title(main="Spectral Profile from RapidEye", font.main = 2)

#set legend
legend("topleft", rownames(ms),
       cex=0.8, col=mycolor, lty = 1, lwd =3, bty = "n")

#========== Supervised Calculation ==============

#----------- mask with the Atkis-Layer --------


atkis_4101_rapid <- st_transform(atkis_4101, crs = projection(rapid))
atkis_4101_rapid <- as(atkis_4101_rapid, "Spatial")
atkis_4101_croprapid <- crop(atkis_4101_rapid, rapid)

#quick look
mapview(rapid) +
  mapview(atkis_4101_croprapid)

rapid_masked <- raster::mask(rapid, atkis_4101_croprapid, progress = "text") # estimated time 10min
mapview(rapid_masked)

#creating dataframe using band data and group name
df_extr <- data.frame(luc12_SP_Ex, luc12_SF[unlist(luc_crop), ]$lc1_group)

#change band names and group column name
names(df_extr) <- c("Blau", "Gruen", "Red", "RedEdge", "NIR", "Group")
names(rapid_masked) <- c("Blau", "Gruen", "Red", "RedEdge", "NIR")
df_extr

#training model
model.class <- rpart(as.factor(Group)~., data = df_extr, method = 'class')

#which groups are included
table(df_extr$Group)

#print model.class
model.class

#optional: print Classification Tree
plot(model.class, uniform=TRUE, main="Classification Tree")
text(model.class, cex=.8)

#calculate prediction
pr <- predict(rapid_masked, model.class, type='class', progress = 'text')
pr

plot(pr)

#Data export
library(rgdal)

writeRaster(pr, "SVmask.tif")
writeRaster(rapid_masked, "rapidmask.tif")
writeOGR(atkis_4101_croprapid, "Atkis_croprapid", layer="atkis", driver="ESRI Shapefile")
st_write(luc12_SF_bbox, "LucasAtkisRapid", driver = "ESRI Shapefile")




