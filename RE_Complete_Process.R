
#### Cropclassbestimmung eines Tiles
#### RapidEye - Projekt Umweltwissenschaften

#====================== NDVI ============================

#Installing required packages
install.packages("pacman")
pacman::p_load(raster, mapview, rasterVis, devtools)
install.packages("backports")
install.packages("ModelMetrics")
install.packages("caret")
devtools::install_github("krlmlr/here")
devtools::install_github("pat-s/hsdar")

#Loading required packages
library(raster)
library(rasterVis)
library(mapview)
library(hsdar)
require(here)

#-------------- Input -----------------

#Set WD
setwd("C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften")
datadir <- "C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidEye"
file <- list.files(file.path(datadir, "3362507_2012-07-24_RE2_3A_326529"), pattern = ".tif", full.names = TRUE)
# list files
#file <- here("Daten") %>% list.files(pattern = ".tif$", full.names = TRUE)
file

#read the files in R, nlayers shows the number of bands
rapid <- raster::brick(file[1])
rapid

#-------------- Visualization ------------

#Quick look at the image, Number from 1 to 5 can be chosen (5 bands sample)
rasterVis::levelplot(rapid[[4]], margin = FALSE, pretty = TRUE)
mapview(rapid[[4]], na.color = "transparent", map.types = "Esri.WorldImagery")


#-----------Transforming the Rasterclass so hsdar can use it------------

#apply the wavelengthborders from the metadata
#Vector with corresponding wavelength for each band. 

#get.sensor.characteristics optional

wavelength <- c(440, 520, 630, 690, 760)
wavelength

#optional hyperspecs for performance
#hyperspecs <- hsdar::HyperSpecRaster(raster, wavelength)
#class(hyperspecs)

#speclip for further steps
rapidclass <- hsdar::speclib(rapid, wavelength)
class(rapidclass)


#-------------------- Vegetation Indices --------------

#required packages
devtools::install_github("pat-s/rasterFunctions")
install.packages("colorRamps")
install.packages("tiff")
require(rasterFunctions)
require(colorRamps)


#NDVI Calculation Option 1
time <- Sys.time()
ndvi_speclib <- vegindex(rapidclass, index = "NDVI2")
Sys.time() - time

#Quick look
rasterVis::levelplot(ndvi_speclib@spectra@spectra_ra, margin = FALSE,
                     pretty = TRUE, col.regions = rev(colorRamps::green2red(400)))
#Dataexport
writeRaster(ndvi_speclib, "3362507_2012-07-24_RE2_3A_326529_NDVI", format = "GTiff", overwrite=TRUE)



#================= Principal Component Analysis ===============

set.seed(1)
sr <- sampleRandom(rapid, 10000)
plot(sr[,c(2,4)])

pca <- prcomp(sr, scale = TRUE)
pca
#plot(pca)

pci <- predict(rapid, pca, index = 1:2)
spplot(pci, col.regions = rev(heat.colors(20)), cex=1,
       main = list(label="First 2 principal components from Rapideye"))

#====================== Unsupervised Classification =============

gc() #Arbeitsspeicher leeren
km <- kmeans(values(rapid), centers=5, iter.max=500,
             nstart=3, algorithm="Lloyd")     #High Memory Usage
kmr <- setValues(ndvi_speclib, km$cluster)
plot(kmr, main = 'Unsupervised classification of RapidEye')

#==================== Supervised Classification =============

#-------- LUCAS DATA PREPARATION ----------------

#### data ####
## regions, catchments, atkis
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

dbDisconnect(con)
dbUnloadDriver(drv)

#### data preparation ####
ezgs = ezgs[which(st_is_valid(ezgs)), ]
atkis_4101 = atkis_4101[which(st_is_valid(atkis_4101)), ]
luc12_SN = st_intersection(luc12_sf, ger_states) # spatially subset luc12 to SN

#### Lucas sites IN atkis_4101 ####
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = DBname_1, user = DBuser, host = DBhost, port = DBport, password = DBpassword)

# this should work with sf:: as well
luc12_SN_4101 = st_read_db(con, query = 
                             "SELECT t1.*
                           FROM spatial_derived.lucas12 t1,
                           (
                           SELECT *
                           FROM \"atkis_DLM\".veg01_f_dtl_31467
                           WHERE oba = 4101 AND land = 'sn' ) t2
                           WHERE ST_Contains(t2.geom, t1.geom)")

dbDisconnect(con)
dbUnloadDriver(drv)

#-------------------------- Training and Classification ---------

#required packages
library(rpart)

# Train the model
df <- data.frame(samp$class, luc12_SN_4101)
model.class <- rpart(as.factor(samp.class)~., data = luc12_SN_4101, method = 'class')

#print
model.class

#optional: Print Classification Tree
plot(model.class, uniform=TRUE, main="Classification Tree")
text(model.class, cex=.8)

# Now predict the subset data based on the model; prediction for entire area takes longer time
pr <- predict(ss, model.class, type='class', progress = 'text')
pr

# Plot the Result
pr <- ratify(pr)
rat <- levels(pr)[[1]]
rat$legend <- c("cloud","forest","crop","fallow","built-up","open-soil","water","grassland")
levels(pr) <- rat
levelplot(pr, maxpixels = 1e6,
          col.regions = c("darkred","cyan","yellow","burlywood","darkgreen","lightgreen","darkgrey","blue"),
          scales=list(draw=FALSE),
          main = "Supervised Classification of Sentinel data")

