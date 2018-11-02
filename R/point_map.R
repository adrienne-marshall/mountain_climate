library(tidyverse)
library(maptools)
library(viridis)
library(ggthemes)

dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Filter to only locations with numbers. 
point_dat <- dat[grepl("[[:digit:]]", dat$location),]

ok_extents <- c("point", "1 - 100 km2", "100 - 1500 km2")

# Only keep cases with extent less than 1500 km2. Keeps 103 studies. 
point_dat <- point_dat %>% 
  filter(extent %in% ok_extents)

# Clean up lat/lon
point_dat <- point_dat %>% 
  unnest_tokens(location, location, token = stringr::str_split, pattern = "; ") %>%
  mutate(location = gsub("[[:alpha:]]", "", location)) %>%
  mutate(location = gsub("\\( | ||)", "", location)) %>%
  mutate(location = gsub("^,+", "", location)) %>% 
  filter(str_length(location) > 2) %>% 
  separate(location, into = c("lat", "lon"), sep = ",", remove = F) %>% 
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  ungroup() %>%
  filter(!is.na(lat), !is.na(lon)) %>% 
  mutate(paper_id = as.factor(paper_id)) 

# Remove points that are not in CRB. 
point_dat <- point_dat %>% filter(lon > -125) %>% filter(lat > 30) %>% 
  filter()

p <- ggplot() + 
  stat_density_2d(data = point_dat,
                  aes(x = lon, y = lat, fill = ..level..),
                  geom = "polygon",
                  show.legend = F, n = 200) +
  scale_fill_viridis(option = "D", alpha = 0.5) + 
  geom_point(data = point_dat, 
             aes(x = lon, y = lat),
             shape = 21, fill = "white", 
             color = "black", show.legend = F) + 
  borders("state", size = 0.4, colour = "grey30") + 
  scale_x_continuous(minor_breaks = seq(-125, -105, 1)) + 
  scale_y_continuous(minor_breaks = seq(40, 53, 1)) + 
  theme_few() +
  theme(panel.grid.minor = element_line(color = "white"),
        panel.grid.major = element_line(color = "grey85"),
        axis.title = element_blank()) + 
  coord_map(projection = "albers", lat0 = min(point_dat$lat), lat1 = max(point_dat$lat),
            xlim = c(min(point_dat$lon) - 2, max(point_dat$lon) + 2), 
            ylim = c(min(point_dat$lat) - 2, max(point_dat$lat) + 1))
p

pdf("../results/figures/point_map.pdf")
p
dev.off()


