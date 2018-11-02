# This is just a test - for how many papers in our final corpus do we have abstracts?
library(tidyverse)
library(fuzzyjoin)

raw <- read_csv("../data/papers_inclusion/aggregated_data_Sept8.csv") 

dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Clean titles
clean_titles <- function(df){
  df %>% 
    mutate(title = tolower(title)) %>% 
    mutate(title = str_trim(title)) %>% 
    mutate(title = str_replace(title, "\\.", ""))
}

raw <- clean_titles(raw)
dat <- clean_titles(dat)

# How many titles are a perfect match? 
dat %>% filter(title %in% raw$title) %>% nrow() 
# 562

# What's missing? 
missing <- dat %>% filter(!title %in% raw$title) %>% dplyr::select(title) %>% 
  mutate(first_three = word(title, 1, 3))

# Look for possible matches. 
poss_match <- missing %>% 
  split(.$title) %>% 
  purrr::map(function(df, raw_df){
    x <- raw_df %>% filter(str_detect(title, df$first_three[1]))
    x <- x$title
    return(x)
  }, raw)

# Change raw titles to match dat titles as needed.
# make selections manually.
select_df <- data.frame(index = 1:length(poss_match),
                     dat_titles = names(poss_match),
                     choices = c(1, 1, NA, 5, NA, 1, 1, NA, 3, 1, 1, 1, 1, 1, 1))

for(i in 1:nrow(select_df)){
  raw$title[raw$title == poss_match[i]][select_df$choices[i]] <- names(poss_match[i])
}

# Now how many titles are a perfect match?
dat %>% filter(title %in% raw$title) %>% nrow() # 570. 

# Join them and get abstracts for these.
abstract_dat <- dat %>% dplyr::select(title) %>% 
  filter(title %in% raw$title) %>% 
  inner_join(raw %>% dplyr::select(abstract, title), 
            by = "title") %>% 
  distinct(title, .keep_all = T) %>% 
  filter(str_length(abstract) >= 10)
# 517 abstracts. 

write_csv(abstract_dat, "../results/tabular/included_abstracts.csv")

