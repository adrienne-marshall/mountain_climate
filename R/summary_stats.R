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

