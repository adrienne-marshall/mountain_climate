# Make a figure that shows how many papers are in our corpus over time. 

library(tidyverse)

# Get data - should be incorporating both sets. 
dat <- read_csv("../results/tabular/single_copy_results.csv") %>%
  rename("final_id" = "paper_id")
corpus <- read_csv("../data/final_lists/final_corpus.csv")

dat <- left_join(dat, corpus[,c("final_id", "year")], by = "final_id")

dat <- dat %>%
  mutate_all(tolower)

annual <- dat %>% group_by(year) %>% 
  filter(year < 2017) %>%
  count() %>% 
  ungroup() %>%
  filter(!is.na(year)) %>%
  mutate(year = as.numeric(year))

p <- ggplot(annual, aes(x = year, y = n)) + 
  geom_point() +
  geom_line() +
  theme_few() + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  labs(x = "Year", y = "Number of papers")

tiff("../results/figures/publications_per_year.tiff", 
     res = 300, units = 'in', width = 9, height = 6)
p
dev.off()
