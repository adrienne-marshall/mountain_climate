# Extents bar chart. 
# This will probably need adjustment for new factor levels.
library(tidyverse)

# Get data. 
dat <- read_csv("../results/tabular/single_copy_results.csv")

# Group by extent and plot.
extent_dat <- dat %>%
  group_by(extent) %>%
  count(sort = T) %>%
  filter(!is.na(extent))

# Add a column to order extent. 
extent_orders <- tribble(
  ~extent, ~order,
  "western us", 1,
  "pacific northwest \\(e.g.,", 2,
  "larger than 40", 3,
  "25,000 km2 -", 4,
  "1500 km2 - ", 5, 
  "500 km2 - ", 6,
  "100 km2 - ", 7,
  ">10km2", 8,
  ">1km2", 9,
  "900m2 - ", 10,
  "point or plot", 11
)

# Add orders based on column content.
extent_dat <- regex_left_join(extent_dat, extent_orders,
                               by = "extent") %>%
  select(-extent.x) %>%
  rename("extent" = "extent.y") %>%
  filter(!is.na(order))

# Make a plot - should it be colored somehow? 
p <- ggplot(extent_dat, 
       aes(x = fct_reorder(extent, order, .desc = TRUE), 
           y = n)) + 
    geom_col() +
    coord_flip() 

pdf("../results/figures/extent_bar.pdf",
    width = 9, height = 6)
p
dev.off()

