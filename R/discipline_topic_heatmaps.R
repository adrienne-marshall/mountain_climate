# For several of the top disciplines, make heatmaps subset by discipline. 
library(tidyverse)
library(tidytext)
library(ggthemes)
library(ggraph)
library(igraph)
library(widyr)
library(cowplot)

# Get data - should ultimately be combination of first and second round. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Subset to only papers that are in disciplines of interest. 
discipline_dat <- dat %>% 
  unnest_tokens(input = discipline, output = discipline, token = stringr::str_split, pattern = ",") %>%
  mutate(discipline = str_trim(discipline)) %>% 
  filter(discipline %in% c("biology/ecology",
                           "hydrology",
                           "meteorology and climatology",
                           "forestry",
                           "policy or management")) %>% 
  split(.$discipline)

# For each discipline, make a pairwise count of topics. 
topic_pairs <- purrr::map(discipline_dat, function(df){
  ans <- df %>% 
    unnest_tokens(input = topic, output = topic, token = stringr::str_split, pattern = ",") %>% 
    filter(!topic %in% c("na", "other")) %>% 
    mutate(topic = str_trim(topic)) %>% 
    pairwise_count(topic, paper_id, sort = T, upper = T, diag = T) 
  
  # expand.grid to include all posisbilities.
  all_poss <- expand.grid(item1 = unique(df$topic),
                          item2 = unique(df$topic))
  
  ans <- left_join(ans, all_poss) %>% 
    mutate(discipline = unique(df$discipline))
})

# Make data frames for topic orders. ----------
topic_orders <- purrr::map(discipline_dat, function(df){
  df %>% 
    unnest_tokens(input = topic, output = topic, token = stringr::str_split, pattern = ",") %>% 
    mutate(topic = str_trim(topic)) %>% 
    filter(!topic %in% c("na", "other")) %>%
    group_by(topic) %>% 
    count(sort = T) %>% 
    rename(item_freq = n)
})

# Make plots -------------
plots <- purrr::map2(topic_pairs, topic_orders, function(pair_df, order_df){
  plot_dat <- pair_df %>% 
    left_join(order_df, by = c("item1" = "topic")) %>% 
    rename(item1_freq = item_freq) %>% 
    left_join(order_df, by = c("item2" = "topic")) %>%
    rename(item2_freq = item_freq) %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(item1 = fct_reorder(item1, item1_freq)) %>%
    mutate(item2 = fct_reorder(item2, item2_freq)) %>% 
    filter(!item1 %in% c("other", "na", "temperature", "precipitation", "mathematics"),
           !item2 %in% c("other", "na", "temperature", "precipitation", "mathematics")) 
  
  # Get rid of lowest 10% of topics. 
  plot_dat <- plot_dat %>% 
    filter(item1_freq > quantile(plot_dat$item1_freq, 0.1),
           item2_freq > quantile(plot_dat$item2_freq, 0.1))
  
  
  p <- ggplot(plot_dat, aes(x = item1, y = item2,
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
          axis.ticks = element_blank()) + 
    labs(title = unique(plot_dat$discipline))
  
  if(unique(plot_dat$discipline == "biology/ecology")){
    plot_dat$discipline <- "biology_ecology"
  }
  
  pdf(paste0("../results/figures/", unique(plot_dat$discipline), "_topic_heatmap.pdf"),
      width = 9, height = 7)
  print(p)
  dev.off()
  
  return(p)
})

plots[[3]]


