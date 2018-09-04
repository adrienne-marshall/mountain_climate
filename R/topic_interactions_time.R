# Look at how interactions between topics have changed over time.
library(tidyverse)
library(tidytext)
library(widyr)
# library(directlabels)

# Include temperature and precip?
temp_precip <- TRUE

# Topic or discipline? 
case <- "discipline"

if(case == "discipline"){
  n1 <- 6
} else {n1 <-  20}

# Get data we coded. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv") 
names(dat)[names(dat) == case] <- "col1"

# Get paper metadata (for year).
metadata <- read_csv("../data/final_lists/final_corpus.csv") %>% 
  dplyr::select(final_id, year) %>% 
  filter(final_id %in% dat$paper_id) %>% 
  rename(paper_id = final_id)

dat <- left_join(dat, metadata)

# Get topics.
col1_df <- dat %>%
  dplyr::select(paper_id, col1, year) %>%
  unnest_tokens(input = col1, output = col1, token = stringr::str_split, pattern = ",") %>%
  mutate(col1 = str_trim(col1)) %>%
  filter(!is.na(col1)) %>% 
  filter(col1 != "pdo)")

if(temp_precip == FALSE){
  col1_df <- col1_df %>% filter(!col1 %in% c("temperature", "precipitation"))
}

# Get pairs - subset by top col1s; count by year. 
col1_pairs <- col1_df %>%
  group_by(year) %>% 
  pairwise_count(col1, paper_id, sort = TRUE, upper = FALSE) %>% 
  mutate(name = paste0(item1, " / ", item2)) %>% 
  filter(year < 2017)

# Filter to most common combinations. 
top_pairs <- col1_pairs %>% 
  group_by(name) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  top_n(n1, nn)

plot_dat <- col1_pairs %>% 
  filter(name %in% top_pairs$name) %>% 
  mutate(x_lab_pos = 2017)

# Make a plot. 
p <- ggplot(plot_dat,
       aes(x = year, y = n, color = name, group = name)) + 
  geom_smooth(show.legend = F, linetype = 2, size = 0.5, alpha = 0.2) + 
   geom_line(show.legend = F) +
  facet_wrap(~name, scales = "free_y", ) + 
  theme_few() + 
  theme(text = element_text(size = 8),
        panel.border = element_blank())
p

if(temp_precip == T){
  filename <- paste0(case, "_trends_temp_precip")
} else {filename <- paste0(case, "_trends")
}

pdf(paste0("../results/figures/", filename, ".pdf"),
    width = 9, height = 6)
p
dev.off()

