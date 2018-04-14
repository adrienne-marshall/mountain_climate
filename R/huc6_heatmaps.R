# Make topic and discipline heatmaps with HUC6. 

library(tidyverse)
library(tidytext)
library(ggthemes)
library(viridis)
library(cowplot)

# Get data - should ultimately be combination of first and second round. 
dat <- read_csv("../results/tabular/single_copy_results.csv")

# Unnest topic and discipline. 
topic_df <- dat %>% 
  unnest_tokens(topic, topic, token = stringr::str_split, pattern = ", ") 

# Get top n topics to plot.
n_topics <- 10
top_topics <- topic_df %>%
  group_by(topic) %>%
  count(sort = T) %>%
  ungroup() %>%
  top_n(n_topics)

# Unnest HUC6 to count HUC6*topic. 
topic_df <- topic_df %>%
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>%
  mutate(huc6 = ifelse(grepl("pend orielle", huc6), 
                       gsub("pend orielle", "pend oreille", huc6), 
                       huc6)) %>%
  filter(topic %in% top_topics$topic) %>%
  group_by(huc6, topic) %>%
  count()

huc6_topics <- expand.grid(topic = unique(top_topics$topic),
                           huc6 = unique(topic_df$huc6))
huc6_topics <- left_join(huc6_topics, topic_df)
huc6_topics$n[is.na(huc6_topics$n)] <- 0

# Keep only text for HUC6. 
huc6_topics <- huc6_topics %>%
  mutate(huc6 = gsub("[[:digit:]]+ - ", "", huc6))

# Get rid of those that contain the term "all" or "both".
huc6_topics <- huc6_topics %>%
  filter(!grepl("all", huc6)) %>%
  filter(!grepl("both", huc6))

# Make a plot. 
# First, arrange by total N for HUC6 and topic. 
topic_plot <- ggplot(huc6_topics, aes(x = fct_reorder(topic, n), 
                        y = fct_reorder(huc6, n), 
                        fill = n)) + 
  geom_tile() + 
  scale_fill_viridis(option = "A") + 
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 50, hjust = 1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

## Repeat for discipline. ---------------
# Unnest discipline. 
discipline_df <- dat %>% 
  unnest_tokens(discipline, discipline, token = stringr::str_split, pattern = ", ") 

# Get top n disciplines to plot.
n_disciplines <- 10
top_disciplines <- discipline_df %>%
  group_by(discipline) %>%
  count(sort = T) %>%
  ungroup() %>%
  top_n(n_disciplines)

# Unnest HUC6 to count HUC6*discipline. 
discipline_df <- discipline_df %>%
  unnest_tokens(huc6, huc6, token = stringr::str_split, pattern = ", ") %>%
  mutate(huc6 = ifelse(grepl("pend orielle", huc6), 
                       gsub("pend orielle", "pend oreille", huc6), 
                       huc6)) %>%
  filter(discipline %in% top_disciplines$discipline) %>%
  group_by(huc6, discipline) %>%
  count()

huc6_disciplines <- expand.grid(discipline = unique(top_disciplines$discipline),
                           huc6 = unique(discipline_df$huc6))
huc6_disciplines <- left_join(huc6_disciplines, discipline_df)
huc6_disciplines$n[is.na(huc6_disciplines$n)] <- 0

# Keep only text for HUC6. 
huc6_disciplines <- huc6_disciplines %>%
  mutate(huc6 = gsub("[[:digit:]]+ - ", "", huc6))

# Get rid of those that contain the term "all" or "both".
huc6_disciplines <- huc6_disciplines %>%
  filter(!grepl("all", huc6)) %>%
  filter(!grepl("both", huc6))

# Make a plot. 
# First, arrange by total N for HUC6 and discipline. 
discipline_plot <- ggplot(huc6_disciplines, aes(x = fct_reorder(discipline, n), 
                                      y = fct_reorder(huc6, n), 
                                      fill = n)) + 
  geom_tile() + 
  scale_fill_viridis(option = "A") + 
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 50, hjust = 1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
#discipline_plot

p <- plot_grid(topic_plot, discipline_plot, nrow = 1,
          labels = "auto", label_size = 10)

pdf("../results/figures/topic_discipline_huc_heatmap.pdf",
    width = 10, height = 5)
p
dev.off()

