# Get HUC6 from national watershed boundary database (3GB file); save to project folder.


require(rgdal)
require(maptools)
require(tidyverse)
library(raster)
#library(data.table)
library(reshape2)
library(sf)

# The input file geodatabase
fgdb <- "/Volumes/research_storage/interannual/data/spatial/WBD/WBD.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)

# Read the HUC6 feature class as sf object. 
huc6 <- st_read(dsn = fgdb, layer = "WBDHU6")

# Get HUC2 so we can subset HUC6 to CRB.
huc2 <- st_read(dsn = fgdb, layer = "WBDHU2")
crb <- huc2 %>% filter(NAME == "Pacific Northwest Region")

# Select only within CRB; relevant columns. 
crb_huc6 <- st_intersection(huc6, crb)
crb_huc6 <- crb_huc6 %>%
  filter(str_sub(HUC6, 1, 2) == "17") %>%
  dplyr::select(AREASQKM, STATES, HUC6, NAME, SHAPE)


st_write(crb_huc6, dsn = "../data/spatial/crb_huc6.shp",
         layer = "crb_huc6.shp", driver = "ESRI Shapefile")


# # Read the HUC6 feature class and make a data frame from it. 
# fc <- readOGR(dsn=fgdb,layer="WBDHU6")
# fc@data$id = rownames(fc@data)
# 
# 
# # Get HUC17. 
# fc2 <- readOGR(dsn = fgdb, layer = "WBDHU2")
# fc2@data$id = rownames(fc2@data)
# fc17 <- fc2[fc2@data$HUC2 == 17,]
# 
# # Crop HUC6 to HUC17. 
# huc6_crb <- crop(fc, fc17)
# huc6.points = fortify(huc6_crb, region = "id") #takes several seconds
# huc6.df = full_join(huc6.points, fc@data, by = "id")
# huc6.df <- huc6.df %>%
#   dplyr::select(long, lat, order, hole, piece, id, group, AREASQKM, HUC6, NAME)
# 
# write.csv(huc6.df, "../data/spatial/huc6_df.csv")
# 
# 
