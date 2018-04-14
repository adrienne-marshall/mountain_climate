# The point of this file is to map summary statistics across HUC6 unites. 

library(tidyverse)
library(tidytext)
library(sf)
library(ggthemes)

# Get data - should ultimately be combination of first and second round. 
dat <- read_csv("../results/tabular/single_copy_results.csv")

# Get HUC6 data. 
huc6_raw <- st_read("../data/spatial/crb_huc6.shp")
huc6_raw <- st_transform(huc6_raw, 3857)
huc6 <- st_simplify(huc6_raw)

# Unnest topic and discipline. 
topic_df <- dat %>% 
  unnest_tokens(topic, topic, token = stringr::str_split, pattern = ", ") 

# Get top n topics to plot.
n_topics <- 4
top_topics <- topic_df %>%
  group_by(topic) %>%
  count(sort = T) %>%
  ungroup() %>%
  top_n(n_topics)

# Unnest HUC6 to count HUC6*topic. 
topic_df <- topic_df %>%
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>%
  filter(topic %in% top_topics$topic) %>%
  group_by(huc6, topic) %>%
  count()

huc6_topics <- expand.grid(topic = unique(top_topics$topic),
                           huc6 = unique(topic_df$huc6))
huc6_topics <- left_join(huc6_topics, topic_df)
huc6_topics$n[is.na(huc6_topics$n)] <- 0

# Get HUC6 to match names in shapefile. 
huc6_topics <- huc6_topics %>%
  mutate(huc6_id = str_sub(huc6, 1, 4)) 
huc6_topics$huc6_id[grep("[[:digit:]]", huc6_topics$huc6_id)] <- paste0(17, huc6_topics$huc6_id[grep("[[:digit:]]", huc6_topics$huc6_id)])

# Join with shapefile. 
huc6_topics <- rename(huc6_topics, "HUC6" = "huc6_id")
plot_dat <- left_join(huc6, huc6_topics, by = "HUC6")

# Make a plot. 
ggplot(plot_dat) + 
  geom_sf(aes(fill = n)) + 
  facet_wrap(~topic) + 
  theme_few()
