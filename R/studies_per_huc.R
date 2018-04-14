# Make a map to show total number of studies in each HUC.
# Subset to studies that are HUC-specific. 

library(tidyverse)
library(sf)
library(ggthemes)

# Get data - should ultimately be combination of first and second round. 
dat <- read_csv("../results/tabular/single_copy_results.csv")

# Get HUC6 data. 
huc6_raw <- st_read("../data/spatial/crb_huc6.shp")
#laea <- st_crs("+proj=laea +lat_0=30 +lon_0=-95")
huc6_raw <- st_transform(huc6_raw, 4326)
huc6 <- st_simplify(huc6_raw, dTolerance = 0.1)

# Count studies per huc.
huc_dat <- dat %>%
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>% 
  group_by(huc6) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(huc6 = str_sub(huc6, 1, 4)) 
huc_dat$huc6[grep("[[:digit:]]", huc_dat$huc6)] <- paste0(17, huc_dat$huc6[grep("[[:digit:]]", huc_dat$huc6)])

# Get rid of all/both HUC designations.
huc_dat <- huc_dat %>%
  filter(!is.na(huc6)) %>%
  filter(!grepl("all", huc6)) %>%
  filter(!grepl("both", huc6)) %>%
  rename("HUC6" = "huc6")

huc6 <- left_join(huc6, huc_dat)

pdf("../results/figures/studies_per_huc.pdf", width = 12, height = 8)
plot(huc6[5],
     main = "Studies per HUC6",
     graticule = TRUE)
dev.off()

# Normalize by area. 
huc6 <- huc6 %>% mutate(n_per_area = 1000*n/AREASQKM)
pdf("../results/figures/studies_per_huc_area.pdf", width = 12, height = 8)

p2 <- plot(huc6[6],
           main = "Studies per HUC6 - area normalized",
           graticule = TRUE)
dev.off()



