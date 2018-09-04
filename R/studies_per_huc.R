# Make a map to show total number of studies in each HUC.
# Subset to studies that are HUC-specific. 

library(tidyverse)
library(sf)
library(ggthemes)
library(tidytext)

# Get data - should ultimately be combination of first and second round. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Get HUC6 data. 
huc6_raw <- st_read("../data/spatial/crb_huc6.shp")
huc6_raw <- st_transform(huc6_raw, 3857)
huc6 <- st_simplify(huc6_raw)

# Change HUC6 projection to Albers. 
huc6 <- st_transform(huc6, "+proj=aea +lat_1=43 +lat_2=48 +lat_0=34 +lon_0=-120 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

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
  rename("HUC6" = "huc6")

huc6 <- left_join(huc6, huc_dat) %>% 
  filter(!is.na(n))

p <- ggplot(huc6) + 
  geom_sf(aes(fill = n), size = 0.1) + 
  scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1],
                       na.value = "#FFFFFF") + 
  theme_few() + 
  theme(axis.text = element_text(size = 7)) +
  labs(fill = "Number \nof \npapers")
p

pdf("../results/figures/studies_per_huc.pdf", width = 12, height = 8)
p
dev.off()

# Normalize by area. 
huc6 <- huc6 %>% mutate(n_per_area = 1000*n/AREASQKM)
p2 <- ggplot(huc6) + 
  geom_sf(aes(fill = n_per_area), size = 0.1) + 
  scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1],
                       na.value = "#FFFFFF") + 
  theme_few() + 
  theme(axis.text = element_text(size = 7)) +
  labs(fill = "Studies \nper \nHUC6 (n/Km2)") 
p2


pdf("../results/figures/studies_per_huc_area.pdf", width = 12, height = 8)
p2 
dev.off()



