library(tidyverse)
library(maptools)
library(viridis)

states <- map_data('state') %>%
  subset(region %in% c("washington", "oregon", "idaho",
                       "montana", "wyoming"))

dat <- read_csv("../results/tabular/single_copy_results.csv")

point_dat <- dat[grepl("[[:digit:]]", dat$location),]

ok_extents <- c("500 km2 - 1500 km2 (HUC 10 is 587 km2)",
                "100 km2 - 500 km2 (HUC 12 is 100 km2)",
                ">10km2 - 100km2 (smaller than HUC 12)",
                ">1km2 - 10km2 (city of Moscow is 2.6 km2)",
                "900m2 - 1km2",
                "1-900 m2 (1-30m; Landsat size or smaller)",
                "Point or plot scale")

#point_dat <- point_dat %>% 
#  filter(extent %in% ok_extents)

# Clean up lat/lon
point_dat <- point_dat %>% 
  unnest_tokens(location, location, token = stringr::str_split, pattern = "; ") %>%
  mutate(location = gsub("[[:alpha:]]", "", location)) %>%
  mutate(location = gsub("\\( | ||)", "", location)) %>%
  mutate(location = gsub("^,+", "", location)) %>%
  separate(location, into = c("lat", "lon"), sep = ",", remove = F) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  ungroup() %>%
  filter(!is.na(lat), !is.na(lon)) %>% 
  mutate(paper_id = as.factor(paper_id)) 

point_dat <- point_dat %>% filter(lon > -125) %>%
  filter(lat > 30)

ggplot() + 
  stat_density_2d(data = point_dat, 
                  aes(x = lon, y = lat, fill = ..level..),
                  geom = "polygon",
                  show.legend = F) + 
  scale_fill_viridis(option = "D") + 
  geom_point(data = point_dat, 
             aes(x = lon, y = lat),
             shape = 21, fill = "white", color = "black", show.legend = F) + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black") +
  scale_x_continuous(minor_breaks = seq(-125, -105, 1)) + 
  scale_y_continuous(minor_breaks = seq(40, 53, 1)) + 
  theme_few() +
  theme(panel.grid.minor = element_line(color = "white"),
        panel.grid.major = element_line(color = "grey95")) + 
  coord_map(projection = "mollweide")
