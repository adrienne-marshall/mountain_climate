# Get HUC6 from national watershed boundary database (3GB file); save to project folder.


require(rgdal)
require(maptools)
require(tidyverse)
library(raster)
#library(data.table)
library(reshape2)

# The input file geodatabase
fgdb <- "/Volumes/research_storage/interannual/data/spatial/WBD/WBD.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)

# Read the HUC6 feature class and make a data frame from it. 
fc <- readOGR(dsn=fgdb,layer="WBDHU6")
fc@data$id = rownames(fc@data)


# Get HUC17. 
fc2 <- readOGR(dsn = fgdb, layer = "WBDHU2")
fc2@data$id = rownames(fc2@data)
fc17 <- fc2[fc2@data$HUC2 == 17,]

# Crop HUC6 to HUC17. 
huc6_crb <- crop(fc, fc17)
huc6.points = fortify(huc6_crb, region = "id") #takes several seconds
huc6.df = full_join(huc6.points, fc@data, by = "id")
huc6.df <- huc6.df %>%
  dplyr::select(long, lat, order, hole, piece, id, group, AREASQKM, HUC6, NAME)

write.csv(huc6.df, "../data/spatial/huc6_df.csv")


