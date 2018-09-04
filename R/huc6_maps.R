# The point of this file is to map summary statistics for topic and discipline across HUC6 units. 

library(tidyverse)
library(tidytext)
library(sf)
library(ggthemes)
library(pals)
library(cowplot)


# Get data - should ultimately be combination of first and second round. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Get HUC6 data. 
huc6_raw <- st_read("../data/spatial/crb_huc6.shp")
huc6_raw <- st_transform(huc6_raw, 3857)
huc6 <- st_simplify(huc6_raw)

# Change HUC6 projection to Albers. 
huc6 <- st_transform(huc6, "+proj=aea +lat_1=43 +lat_2=48 +lat_0=34 +lon_0=-120 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

# Unnest topic. 
topic_df <- dat %>% 
  unnest_tokens(topic, topic, token = stringr::str_split, pattern = ", ") 

# Get top n topics to plot.
n_topics <- 6
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
plot_dat <- left_join(huc6, huc6_topics, by = "HUC6") %>% 
  filter(!is.na(topic))

# Make a plot. 
p1 <- ggplot(plot_dat) + 
  geom_sf(aes(fill = n), size = 0.1) + 
  scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1]) + 
  facet_wrap(~topic) + 
  theme_few() + 
  theme(axis.text = element_text(size = 7)) +
  labs(fill = "Number \nof \npapers")
# p1

pdf("../results/figures/topic_huc.pdf",
    width = 9, height = 6)
p1
dev.off()

# Make a proportional version. 
n_per_huc <- dat %>% 
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>%
  group_by(huc6) %>% 
  count() %>% 
  rename(total_papers = n)

plot_dat2 <- plot_dat %>% 
  left_join(n_per_huc, by = "huc6") %>% 
  mutate(n_percent = 100*n/total_papers)

tops <- unique(plot_dat2$topic)
plots <- vector("list", length(tops))

for(i in 1:length(plots)){
  plots[[i]] <- ggplot(plot_dat2 %>% 
                         filter(topic == tops[i])) + 
    geom_sf(aes(fill = n_percent), size = 0.1) + 
    scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1]) + 
    facet_wrap(~topic) + 
    theme_map() + 
    theme(axis.text = element_text(size = 7),
          legend.key.size = unit(0.7, "line"),
          legend.text = element_text(size = 8)) +
    labs(fill = "")
}

p1a <- plot_grid(plotlist = plots, nrow = 2, ncol = 3)
pdf("../results/figures/topic_huc_proportional.pdf",
    width = 9, height = 6)
p1a
dev.off()

# Repeat for disciplines. ------------
discipline_df <- dat %>% 
  unnest_tokens(discipline, discipline, token = stringr::str_split, pattern = ", ") 

# Get top n disciplines to plot.
n_disciplines <- 6
top_disciplines <- discipline_df %>%
  group_by(discipline) %>%
  count(sort = T) %>%
  ungroup() %>%
  top_n(n_disciplines)

# Unnest HUC6 to count HUC6*discipline. 
discipline_df <- discipline_df %>%
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>%
  filter(discipline %in% top_disciplines$discipline) %>%
  group_by(huc6, discipline) %>%
  count()

huc6_disciplines <- expand.grid(discipline = unique(top_disciplines$discipline),
                           huc6 = unique(discipline_df$huc6))
huc6_disciplines <- left_join(huc6_disciplines, discipline_df)
huc6_disciplines$n[is.na(huc6_disciplines$n)] <- 0

# Get HUC6 to match names in shapefile. 
huc6_disciplines <- huc6_disciplines %>%
  mutate(huc6_id = str_sub(huc6, 1, 4)) 
huc6_disciplines$huc6_id[grep("[[:digit:]]", huc6_disciplines$huc6_id)] <- paste0(17, huc6_disciplines$huc6_id[grep("[[:digit:]]", huc6_disciplines$huc6_id)])

# Join with shapefile. 
huc6_disciplines <- rename(huc6_disciplines, "HUC6" = "huc6_id")
plot_dat <- left_join(huc6, huc6_disciplines, by = "HUC6") %>% 
  filter(!is.na(discipline))

# Make a plot. 
p1 <- ggplot(plot_dat) + 
  geom_sf(aes(fill = n), size = 0.1) + 
  scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1]) + 
  facet_wrap(~discipline) + 
  theme_few() +
  theme(axis.text = element_text(size = 7)) +
  labs(fill = "Number \nof \npapers")
# p1

pdf("../results/figures/discipline_huc.pdf",
    width = 9, height = 6)
p1
dev.off()

# Make a proportional version. 
n_per_huc <- dat %>% 
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>%
  group_by(huc6) %>% 
  count() %>% 
  rename(total_papers = n)

plot_dat2 <- plot_dat %>% 
  left_join(n_per_huc, by = "huc6") %>% 
  mutate(n_percent = 100*n/total_papers)

disc <- unique(plot_dat2$discipline)
plots <- vector("list", length(disc))

for(i in 1:length(plots)){
  plots[[i]] <- ggplot(plot_dat2 %>% 
                         filter(discipline == disc[i])) + 
    geom_sf(aes(fill = n_percent), size = 0.1) + 
    scale_fill_gradientn(colors = pals::ocean.haline(100)[100:1]) + 
    facet_wrap(~discipline) + 
    theme_map() + 
    theme(axis.text = element_text(size = 7),
          legend.key.size = unit(0.7, "line"),
          legend.text = element_text(size = 8)) +
    labs(fill = "")
}

p1a <- plot_grid(plotlist = plots, nrow = 2, ncol = 3)
pdf("../results/figures/discipline_huc_proportional.pdf",
    width = 9, height = 6)
p1a
dev.off()

