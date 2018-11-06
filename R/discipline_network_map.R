# This script maps relationships between disciplines. 


library(tidyverse)
library(tidytext)
library(ggthemes)
library(ggraph)
library(igraph)
library(widyr)
library(cowplot)

# Get data. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Get disciplines.
discipline_df <- dat %>%
  dplyr::select(paper_id, discipline) %>%
  unnest_tokens(input = discipline, output = discipline, token = stringr::str_split, pattern = ",") %>%
  mutate(discipline = str_trim(discipline)) %>%
  filter(!is.na(discipline))

# Subset to top disciplines.
top_disciplines <- discipline_df %>%
  group_by(discipline) %>%
  count(sort = T) %>%
  filter(n > 0) # should bin a little further. 

# Get pairs - subset by top disciplines.
discipline_pairs <- discipline_df %>%
  filter(discipline %in% top_disciplines$discipline) %>%
  pairwise_count(discipline, paper_id, sort = TRUE, upper = TRUE)

# Make a network map. 
set.seed(1234)
p1 <- discipline_pairs %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), 
                 edge_colour = "turquoise4", lineend = "round") +
  #geom_edge_density(aes(fill = n)) + 
  # geom_node_point(size = 3) +
  geom_node_label(aes(label = name), repel = F,
                 # point.padding = unit(0.1, "lines"),
                 size = 4) +
  theme_void() 
p1

pdf("../results/figures/discipline_network_graph.pdf", 
    width = 8, height = 6)
p1
dev.off()

# Linear layout.
p2 <- discipline_pairs %>%
  #filter(n > 1) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = 'linear', circular = FALSE) +
  geom_edge_arc(aes(color = n, edge_alpha = n)) +
  scale_color_viridis(option = "A") + 
  geom_node_label(aes(label = name), repel = TRUE,
                  point.padding = unit(0.2, "lines"),
                  size = 2) +
  theme_void()

# pdf("../results/figures/discipline_network_graphs.pdf",
#     width = 12, height = 6)
# plot_grid(p1, p2)
# dev.off()


# Pair with a heatmap.. 
possible_pairs <- expand.grid(item1 = top_disciplines$discipline,
                              item2 = top_disciplines$discipline)

top_disciplines <- top_disciplines %>%
  rename("item_freq" = "n")
plot_dat <- discipline_pairs %>%
  full_join(possible_pairs) %>%
  left_join(top_disciplines, by = c("item1" = "discipline")) %>%
  rename("item1_freq" = "item_freq") %>%
  left_join(top_disciplines, by = c("item2" = "discipline")) %>%
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

pdf("../results/figures/discipline_network_heatmap.pdf",
    width = 10, height = 6)
p3
dev.off()



