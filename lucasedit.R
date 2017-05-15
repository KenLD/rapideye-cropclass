
### Script to edit the LUCAS Data  ###

# Packages
#install.packages("dplyr")
#install.packages("tidyr")
require(dplyr)
require(utils)
require(tidyr)
require(sf)
require(RPostgreSQL)
require(postGIStools)
require(DBI)
require(sp)
library(rgeos)

library(rgdal)

#import the LUCAS Data
wd <- ("C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften")
setwd(wd)
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
                             "SELECT t1.*
                           FROM spatial_derived.lucas12 t1,
                           (
                           SELECT *
                           FROM \"atkis_DLM\".veg01_f_dtl_31467
                           WHERE oba = 4101 AND land = 'sn' ) t2
                           WHERE ST_Contains(t2.geom, t1.geom)")

dbDisconnect(con)
dbUnloadDriver(drv)

#Change DF in table
luc12_SN_4101_tbl <- tbl_df(luc12_SN_4101)

#View the table
View(luc12_SN_4101_tbl)

#Spalten Einordnen und Gruppenspalte hinzufügen
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
##       Obstbau    44744     0.4

luc12_SN_4101_tbl_groups <- luc12_SN_4101_tbl
luc12_SN_4101_tbl_groups$lc1_group <- ifelse(grepl('^E',luc12_SN_4101_tbl$lc1), "Grasanbau", 
                                        ifelse(grepl('B55',luc12_SN_4101_tbl$lc1), "Grasanbau",
                                             ifelse(grepl('B16',luc12_SN_4101_tbl$lc1), "Mais", 
                                                    ifelse(grepl('^B1',luc12_SN_4101_tbl$lc1), "Getreide",
                                                           ifelse(grepl('B32',luc12_SN_4101_tbl$lc1), "Raps", 
                                                                  ifelse(grepl('B22',luc12_SN_4101_tbl$lc1), "Zuckerrüben", 
                                                                         ifelse(grepl('B40',luc12_SN_4101_tbl$lc1), "Leguminosen", 
                                                                                ifelse(grepl('B21',luc12_SN_4101_tbl$lc1), "Kartoffeln", 
                                                                                       ifelse(grepl('B82',luc12_SN_4101_tbl$lc1), "Wein",
                                                                                              ifelse(grepl('B82',luc12_SN_4101_tbl$lc1), "Erbsen", 
                                                                                                     ifelse(grepl('B70',luc12_SN_4101_tbl$lc1), "Obst", NA)))))))))))
#View the table
View(luc12_SN_4101_tbl_groups)

#Count the groups
count(luc12_SN_4101_tbl_groups, lc1_group)

#optional: Write the table
write.table(luc12_SN_4101_tbl_groups, "C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidProject/LucasGroups.txt")

#create SpatialPointsDf
luc12_SN_4101_SPDF <- SpatialPointsDataFrame(coords = cbind(luc12_SN_4101_tbl_groups$th_long, luc12_SN_4101_tbl_groups$th_lat), data = luc12_SN_4101_tbl_groups,
                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(luc12_SN_4101_SPDF)


#EXPORT für QGIS
write.xlsx2(luc12_SN_4101_SPDF, "C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidProject/LucasGroups.xls", overwrite = TRUE)
st_write(atkis_4101, 'C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidProject',
         driver = 'Esri Shapefile', layer = 'atkis_data')

#In QGIS Punktelayer erstellt mit allen LUCAS Punkten die im ATKIS Layer liegen
#Anschließend wieder in R einlesen

LucasGroupsAtkis <- readOGR("C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidProject/LucasGroupsAtkis.shp")
View(LucasGroupsAtkis)

