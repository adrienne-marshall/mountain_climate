# This script maps relationships between topics. 
# We should go through topics and consider binning those that are user-input.

library(tidyverse)
library(tidytext)
library(ggthemes)
library(ggraph)
library(igraph)
library(widyr)
library(cowplot)

# Get data - should ultimately be combination of first and second round. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Get topics.
topic_df <- dat %>%
  dplyr::select(paper_id, topic) %>%
  unnest_tokens(input = topic, output = topic, token = stringr::str_split, pattern = ",") %>%
  mutate(topic = str_trim(topic)) %>%
  filter(!is.na(topic)) %>% 
  filter(topic != "pdo)")

# Subset to top topics.
top_topics <- topic_df %>%
  group_by(topic) %>%
  count(sort = T) %>%
  filter(n > 1)

# Get pairs - subset by top topics.
topic_pairs <- topic_df %>%
  filter(topic %in% top_topics$topic) %>%
  pairwise_count(topic, paper_id, sort = TRUE, upper = TRUE)

# Make a network map. 
set.seed(1234)
p1 <- topic_pairs %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), 
                 edge_colour = "salmon") +
  #geom_edge_density(aes(fill = n)) + 
  geom_node_point(size = 1) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.1, "lines"),
                 size = 3) +
  theme_void()

pdf("../results/figures/topic_network_graph.pdf", 
    width = 6, height = 6)
p1
dev.off()

# Linear layout.
p2 <- topic_pairs %>%
  #filter(n > 1) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = 'linear', circular = FALSE) +
  geom_edge_arc(aes(color = n, edge_alpha = n)) +
  scale_color_viridis(option = "A") + 
  geom_node_label(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines"),
                 size = 2) +
  theme_void()

# pdf("../results/figures/topic_network_graphs.pdf",
#     width = 12, height = 6)
# plot_grid(p1, p2)
# dev.off()


# Pair with a heatmap - lots of garbage here to get factor levels right. 
possible_pairs <- expand.grid(item1 = top_topics$topic,
                              item2 = top_topics$topic)

top_topics <- top_topics %>%
  rename("item_freq" = "n")
plot_dat <- topic_pairs %>%
  full_join(possible_pairs) %>%
  left_join(top_topics, by = c("item1" = "topic")) %>%
  rename("item1_freq" = "item_freq") %>%
  left_join(top_topics, by = c("item2" = "topic")) %>%
  rename("item2_freq" = "item_freq") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(item1 = fct_reorder(item1, item1_freq)) %>%
  mutate(item2 = fct_reorder(item2, item2_freq))

p3 <- ggplot(plot_dat, aes(x = item1, y = item2,
                        fill = n)) +
  geom_tile() + 
  scale_fill_gradient(name = "Count", trans = "log",
                      low = "blue", high = "yellow",
                      na.value = "grey80",
                      breaks = round(quantile(plot_dat$n, na.rm = T)),
                      labels = round(quantile(plot_dat$n, na.rm = T))) + 
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 50, hjust = 1),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p3

pdf("../results/figures/topic_network_heatmap.pdf",
    width = 10, height = 6)
p3
dev.off()



