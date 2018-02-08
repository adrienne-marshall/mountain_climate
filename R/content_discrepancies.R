# Identify discrepancies in the "coding for content" document.
# Author: Adrienne Marshall
# November 6, 2017

library(tidyverse)

# Get data. 
dat <- read_csv("coding_for_content_version_control/coding_for_content_1106.csv")

names(dat)[4] <- "id"
dat <- dat %>% arrange(id)

# Only keep cases with more than one iteration:
ids <- dat %>% group_by(id) %>% count() %>% filter(n > 1)

dat <- dat %>% filter(id %in% ids$id)

# Melt the data.

