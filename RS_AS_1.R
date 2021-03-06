require(sf)
require(RPostgreSQL)
require(postGIStools)
require(DBI)
wd <- ("C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften")
setwd(wd)
prj = "C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidProject" # Enter path to DB_access.R here!
RSdatadir = 'C:/Users/Ken Mauser/Desktop/Studium Landau/Projekt Umweltwissenschaften/RapidEye'
source(file.path(prj, "src", "DB_access_ken.R"))

#### data ####
## regions, catchments, atkis
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = DBname_1, user = DBuser, host = DBhost, port = DBport, password = DBpassword)

ger_states = st_read_db(con, table = c("spatial_derived", "germany_bld"))
ger_states = ger_states[ger_states$gen == 'Sachsen', ]

# OLD: ger_states <- get_postgis_query(con, "SELECT * FROM spatial_derived.germany_bld", geom_name = 'geom')
ezgs = st_read_db(con, query = "SELECT * 
                  FROM spatial_derived.land_use_ezg30
                  WHERE substring(site_id, 1, 2) = 'SN'")
# OLD: ezgs <- get_postgis_query(con, "SELECT * 
#                                 FROM spatial_derived.land_use_ezg30
#                                 WHERE substring(site_id, 1, 2) = 'SN'",
#                           geom_name = 'geom')

atkis_4101 = st_read_db(con, query = "SELECT *
                        FROM \"atkis_DLM\".veg01_f_dtl_31467
                        WHERE oba = '4101' AND land = 'sn';")

dbDisconnect(con)
dbUnloadDriver(drv)


# Read and upload LUCAS data to DB
# ## lucas data
# lucas12 = read.csv(file.path(RSdatadir, "DE_2012_20160926.csv"), stringsAsFactors = FALSE)
# coordinates(lucas12) = ~GPS_LONG+GPS_LAT
# proj4string(lucas12) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # this is WGS84
# is.projected(lucas12)
# newproj = st_crs(ger_states)$proj4string
# luc12 = spTransform(lucas12, CRS(newproj))
# is.projected(luc12)
# #luc12@data[ ,c("x", "y")] <- coordinates(luc12)
# luc12_sf = st_as_sf(luc12)
# st_crs(luc12_sf) = 31467
# colnames(luc12_sf) = tolower(colnames(luc12_sf))

# drv = dbDriver("PostgreSQL")
# con = dbConnect(drv, dbname = DBname_1, user = DBuser, host = DBhost, port = DBport, password = DBpassword)
# 
# st_write_db(con, luc12_sf, table = c('spatial_derived', 'lucas12'),
#             geom_name = 'geom', overwrite = TRUE, debug = TRUE)
# 
# dbDisconnect(con)
# dbUnloadDriver(drv)


### END
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


## plot overview
plot(ger_states[ ,1], col = NA)
plot(ezgs[1:100,1], add = T, col = 'red')
plot(atkis_4101[1:100,1], add = T)
# plot(luc12_SN[sample(seq_along(luc12_SN$POINT_ID), 100) ,1],
#      add = T, col = 'purple')
plot(luc12_SN_4101[ ,1], add = T)

       

## LUCAS categories
# http://ec.europa.eu/eurostat/documents/205002/208012/LUCAS2012_C3-Classification_20131004_0.pdf/a71e46b5-a14b-4c9e-83ed-4f973dc139e0
unique(luc12_SN_4101$lc1)
luc12_SN_4101$lc1_name =
  with(luc12_SN_4101,
       ifelse(lc1 == 'B13', lc1_name == 'Barley',
              ifelse(lc1 == 'B11', lc1_name == 'Common wheat',
                     ifelse(lc1 == 'B32', lc1_name == 'rapes',
                            ifelse(lc1 == 'E20', lc1_name == 'Grassland w/o shrubs',
                                   ifelse(lc1 == 'B23', lc1_name == 'Other rootcrops',
                                          ifelse(lc1 == 'B15', lc1_name == 'Oat',
                                                 ifelse(lc1 == 'B16', lc1_name == 'Corn',
                                                        ifelse(lc1 == 'B41', lc1_name == 'Pulses',
                                                               ifelse(lc1 == 'B18', lc1_name == 'Triticale',
                                                                      ifelse(lc1 == 'B55', lc1_name == 'Grassland',
                                                                             ifelse(lc1 == 'B14', lc1_name == 'Rye',
                                                                                    ifelse(lc1 == 'C10', lc1_name == 'Broadleaved Woodland',
                                                                                           ifelse(lc1 == 'A22', lc1_name == 'Artificial Non-built up Areas',
                                                                                                  ifelse(lc1 == 'B22', lc1_name == 'Sugar Beet',
                                                                                                         ifelse(lc1 == 'B51', lc1_name == 'Clovers',
                                                                                                                ifelse(lc1 == 'B53', lc1_name == 'Other Leguminous',
                                                                                                                       ifelse(lc1 == 'E30', lc1_name == 'Spontaneously re-vegetated surfaces',
                                                                                                                              ifelse(lc1 == 'B71', lc1_name == 'Apple Fruit',
                                                                                                                                     ifelse(lc1 == 'B12', lc1_name == 'Durum Wheat',
                                                                                                                                            ifelse(lc1 == 'B21', lc1_name == 'Potatoes',
                                                                                                                                                   ifelse(lc1 == 'B52', lc1_name == 'Lucerne',
                                                                                                                                                          ifelse(lc1 == 'B31', lc1_name == 'Sunflower',
                                                                                                                                                                 ifelse(lc1 == 'C32', lc1_name == 'Pine dominated mixed Woodland',
                                                                                                                                                                        ifelse(lc1 == 'C22', lc1_name == 'Pine dominated coniferous Woodland',
                                                                                                                                                                               ifelse(lc1 == 'A21', lc1_name == 'Artificial Non-built up Areas',
                                                                                                                                                                                      ifelse(lc1 == 'F40', lc1_name == 'Other bare soil',
                                                                                                                                                                                             ifelse(lc1 == 'E10', lc1_name == 'Grassland with sparse tree/shrub cover',
                                                                                                                                                                                                    ifelse(lc1 == 'G20', lc1_name == 'inland running water')))))))))))))))))))))))))))))
                                                                                                                                                                                                           
                                                                                                                                                                 

