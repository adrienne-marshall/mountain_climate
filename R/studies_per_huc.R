# Make a map to show total number of studies in each HUC.
# Subset to studies that are HUC-specific. 

library(tidyverse)
library(sf)
library(ggthemes)
library(tidytext)
library(USAboundaries)
library(patchwork)

# Get data. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Get HUC6 data. 
huc6_raw <- st_read("../data/spatial/crb_huc6.shp")
huc6_raw <- st_transform(huc6_raw, 3857)
huc6 <- st_simplify(huc6_raw)

# Other spatial data: --------

# State boundaries: 
states <- us_states(resolution = "high",
                    states = c("washington", "oregon", "idaho", "wyoming",
                               "montana", "nevada", "utah"))
states <- st_transform(states, crs = st_crs(huc6))


# Rivers: 
source("get_flowlines.R")
rivers <- get_flowlines(streamorder = 7, mapRange = c(-124.9, -110, 42, 50)) # mapRange = st_bbox(huc6_raw)) 
rivers <- st_transform(rivers, crs = st_crs(huc6))
rivers <- st_simplify(rivers, dTolerance = 2000)
rivers <- st_intersection(rivers, huc6)

# st_write(rivers, "../data/spatial/crb_flowlines.shp")

ggplot(rivers) + geom_sf()

# Research watersheds:
# Make sf and transform. 
df <- data.frame(ids =  c("DCEW", "RCEW", "MCEW", "HJA", "PREF"), 
                 x = c(-116.8469, -116.8532, -116.2781, -122.2574, -116.8045),
                 y = c(43.73025, 43.24818, 47.15517, 44.21222, 48.35826)) %>% 
  split(.$ids)
geometry = st_sfc(purrr::map(df, function(df_row) st_point(c(df_row[1,2],
                                                             df_row[1,3]))))
research <- st_sf(id = names(df), 
                  geometry = geometry,
                  crs = 4326)
research <- st_transform(research, crs = st_crs(huc6))

# Make data frame version.
research_df <- data.frame(id = names(df), 
                          x = map_dbl(research$geometry, 1),
                          y = map_dbl(research$geometry, 2))


# Change HUC6 projection to Albers. 
huc6 <- st_transform(huc6, "+proj=aea +lat_1=43 +lat_2=48 +lat_0=34 +lon_0=-120 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

# Count studies per huc.
huc_dat <- dat %>%
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>% 
  group_by(huc6) %>%
  count(sort = T) %>%
  ungroup() %>%
  mutate(huc6 = str_sub(huc6, 1, 4)) %>% 
  filter(huc6 != "1002")
huc_dat$huc6[grep("[[:digit:]]", huc_dat$huc6)] <- paste0(17, huc_dat$huc6[grep("[[:digit:]]", huc_dat$huc6)])

# Get rid of all/both HUC designations.
huc_dat <- huc_dat %>%
  filter(!is.na(huc6)) %>%
  rename("HUC6" = "huc6") 

huc6 <- left_join(huc6, huc_dat) %>% 
  filter(!is.na(n)) %>% 
  filter(HUC6 != "171002")

p <- ggplot(huc6) + 
  geom_sf(aes(fill = n), size = 0.1) + 
  scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1],
                       na.value = "#FFFFFF") + 
  geom_sf(data = states, size = 0.4, fill = NA, color = "grey15") + # states
  geom_sf(data = rivers, color = "red") + # rivers
  geom_point(data = research_df, aes(x = x, y = y), 
             color = "red", size = 1) + # research station points
  geom_label(data = research_df, aes(x = x, y = y, label = id), 
            color = "red", size = 2, hjust = -0.1, alpha = 0.5) + # research station labels
  labs(fill = "Number \nof \npapers") +
  coord_sf(xlim = c(st_bbox(huc6)$xmin, st_bbox(huc6)$xmax),
           ylim = c(st_bbox(huc6)$ymin, st_bbox(huc6)$ymax)) + 
  theme_few() + 
  theme(axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "grey50", size = 0.1),
        axis.title = element_blank()) 
# p

pdf("../results/figures/studies_per_huc.pdf", width = 12, height = 8)
p
dev.off()

# Normalize by area. 
huc6 <- huc6 %>% mutate(n_per_area = 1000*n/AREASQKM)
p2 <- ggplot(huc6) + 
  geom_sf(aes(fill = n_per_area), size = 0.1) + 
  scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1],
                       na.value = "#FFFFFF") + 
  geom_sf(data = states, size = 0.4, fill = NA, color = "grey15") + # states
  geom_sf(data = rivers, color = "red") + # rivers
  geom_point(data = research_df, aes(x = x, y = y), 
             color = "red", size = 1) + # research station points
  geom_label(data = research_df, aes(x = x, y = y, label = id), 
             color = "red", size = 2, hjust = -0.1, alpha = 0.5) + # research station labels
  coord_sf(xlim = c(st_bbox(huc6)$xmin, st_bbox(huc6)$xmax),
           ylim = c(st_bbox(huc6)$ymin, st_bbox(huc6)$ymax)) +
  theme_few() + 
  theme(axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "grey50", size = 0.1),
        axis.title = element_blank()) +
  labs(fill = "Studies \nper \nHUC6 (n/Km2)") 
# p2


pdf("../results/figures/studies_per_huc_area.pdf", width = 12, height = 8)
p2 
dev.off()

pdf("../results/figures/studies_per_huc_combined.pdf", 
    width = 12, height = 6)
p + p2
dev.off()



