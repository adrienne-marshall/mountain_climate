# Get summary statistics. 
require(tidyverse)
library(tidytext)
library(ggthemes)
library(RColorBrewer)

dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Impacts/adaptation/mitigation
IAM <- dat %>%
  group_by(inclusion_IAM) %>%
  count() %>%
  mutate(proportion = n/nrow(dat))
IAM

# Type of impact. --------------
impact_dat <- dat %>% 
  filter(inclusion_IAM == "impacts")

impact_dat %>%
  group_by(projected) %>% 
  count() %>% 
  mutate(percent = 100*n/nrow(impact_dat))
impact_dat %>% 
  group_by(observed) %>% 
  count() %>% 
  mutate(percent = 100*n/nrow(impact_dat))

impact_dat %>% 
  filter(projected == "no" & observed == "no") %>% 
  nrow()
211/507

# Discipline ------------
# Unique disciplines, counting interdisciplinary. 
# First, alphabetize so all are the same. 
dat <- dat %>% 
  unnest_tokens(discipline, discipline, token = stringr::str_split, pattern = ", ") %>%
  arrange(discipline) %>% 
  nest(discipline) %>% 
  mutate(discipline = purrr::map(data, unlist)) %>% 
  mutate(discipline = map_chr(discipline, paste, collapse = ", ")) %>% 
  dplyr::select(-data)

length(unique(dat$discipline))

disc_dat <- dat %>% 
  unnest_tokens(discipline, discipline, token = stringr::str_split, pattern = ", ")

# Single-discipline papers: 
single <- disc_dat %>% group_by(paper_id) %>% count() %>% filter(n == 1) 
nrow(single)
nrow(single)/nrow(dat)

# Multi-discipline: 
multi <- disc_dat %>% group_by(paper_id) %>% count() %>% filter(n > 1) 
nrow(multi)
nrow(multi)/nrow(dat)

# Common single disciplines. 
disc_dat %>% 
  filter(paper_id %in% single$paper_id) %>% 
  group_by(discipline) %>% 
  count(sort = T)

sum(139, 87, 66, 66)/nrow(single)

# Most common multi-disciplines: 
disc_dat %>% 
  filter(paper_id %in% multi$paper_id) %>% 
  group_by(discipline) %>% 
  count(sort = T)

# Most common combinations: 
dat %>% 
  group_by(discipline) %>% 
  count(sort = T) %>% 
  filter(str_detect(discipline, ", "))

# broad summary
disc_dat %>% 
  group_by(discipline) %>% 
  count(sort = T)
sum(209, 165, 126, 110)/nrow(disc_dat)


