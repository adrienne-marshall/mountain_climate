# Bar charts of extents and biomes. 
library(tidyverse)
library(fuzzyjoin)

# Get data. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Group by extent and plot.
extent_dat <- dat %>%
  group_by(extent) %>%
  count(sort = T) %>%
  filter(!is.na(extent)) %>% 
  ungroup()

# Add a column to order extent. 
extent_orders <- tribble(
  ~extent, ~order,
  "western us", 1,
  "pacific northwest", 2,
  "40000 km2 - ", 3,
  "25000 - ", 4,
  "1500 -", 5, 
  "100 km2 - ", 6,
  "1 - 100", 7,
  "point", 8)

# Add orders based on column content.
extent_dat <- regex_left_join(extent_dat, extent_orders,
                               by = "extent") %>%
 #  dplyr::select(-extent.x) %>%
  rename("extent" = "extent.x") %>%
  filter(!is.na(order)) %>% 
  mutate(percent = 100*n/nrow(dat)) %>% 
  dplyr::select(order, extent, n, percent, -extent.y) %>% 
  arrange(order)

extent_dat <- extent_dat %>% 
  mutate(extent = case_when(extent == "western us" ~ "Western U.S.",
                            extent == "pacific northwest" ~ "Pacific Northwest",
                            extent == "point" ~ "Point",
         TRUE ~ extent))

# Make a plot - should it be colored somehow? 
extent_plot <- ggplot(extent_dat, 
       aes(x = fct_reorder(extent, order, .desc = TRUE), 
           y = n)) + 
    geom_col() +
    coord_flip() +
  labs(x = "", y = "Number of papers", title = "(a)") + 
  scale_y_continuous(expand = c(0, 0.9)) + 
  theme_minimal()
extent_plot

pdf("../results/figures/extent_bar.pdf",
    width = 9, height = 6)
extent_plot
dev.off()

# Make a bar chart for biomes. 
biome_count <- dat %>% 
  unnest_tokens(biome, biome, token = stringr::str_split, pattern = ",") %>%
  mutate(biome = str_trim(biome)) %>% 
  mutate(biome = ifelse(biome == "alpine tundra", "alpine/tundra", biome)) %>% 
  group_by(biome) %>%
  count(sort = T) %>% 
  ungroup() %>% 
  filter(!str_detect(biome, "n/a|boreal")) %>% 
  filter(biome != "freshwater")

biome_plot <- ggplot(biome_count, 
                      aes(x = fct_reorder(biome, n), 
                          y = n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Number of papers",
       title = "(b)") + 
  scale_y_continuous(expand = c(0, 0.9)) + 
  theme_minimal()
biome_plot

p <- extent_plot + biome_plot
p

pdf("../results/figures/spatial_bar_charts.pdf",
    width = 8, height = 6)
p
dev.off()