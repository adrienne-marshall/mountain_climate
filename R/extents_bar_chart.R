# Extents bar chart. 
# This will probably need adjustment for new factor levels.
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

# Make a plot - should it be colored somehow? 
p <- ggplot(extent_dat, 
       aes(x = fct_reorder(extent, order, .desc = TRUE), 
           y = n)) + 
    geom_col() +
    coord_flip() +
  labs(x = "", y = "Number of papers")

pdf("../results/figures/extent_bar.pdf",
    width = 9, height = 6)
p
dev.off()

