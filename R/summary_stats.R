# Get summary statistics. 
require(tidyverse)
library(tidytext)
library(ggthemes)
library(RColorBrewer)

dat <- read_csv("../results/tabular/single_copy_results.csv")

# Impacts/adaptation/mitigation
IAM <- dat %>%
  group_by(inclusion_IAM) %>%
  count() %>%
  mutate(proportion = n/nrow(dat))
IAM

# IAM by discipline. 
dat %>% 
  unnest_tokens(discipline, discipline, token = stringr::str_split,
                pattern = ", ") %>%
  group_by(inclusion_IAM, discipline) %>%
  count() %>%
  #filter(n > 1) %>%
  filter(inclusion_IAM %in% c("adaptation", "mitigation")) %>%
  ggplot(aes(x = inclusion_IAM, y = n, fill = discipline)) +
  geom_col(position = "fill") + 
  scale_fill_brewer(type = "qual", palette = "Set3") + 
  theme_few() + 
  theme(axis.title.x = element_blank()) +
  labs(y = "Proportion", fill = "Discipline")
  

# Type of impact. --------------
impact_dat <- dat %>%
  filter(!is.na(impacts)) %>%
  unnest_tokens(impacts, impacts, token = stringr::str_split, 
                pattern = ", ") %>%
  unnest_tokens(impacts, impacts, token = stringr::str_split,
                pattern = "; ") 

impact_dat %>%
  group_by(impacts) %>%
  count() %>%
  mutate(proportion = n/nrow(impact_dat))

# What are the disciplines most associated with different types of impacts?
impact_discipline <- impact_dat %>%
  unnest_tokens(discipline, discipline, token = stringr::str_split, 
                pattern = ", ") %>%
  group_by(impacts, discipline) %>% 
  count() %>%
  arrange(impacts, desc(n))

p <- ggplot(impact_discipline %>% filter(n > 1), 
       aes(x = impacts, y = n, fill = discipline)) +
  geom_col(position = "fill") + 
  scale_fill_brewer(type = "qual", palette = "Set3") + 
  theme_few() + 
  theme(axis.title.x = element_blank()) +
  labs(y = "Proportion", fill = "Discipline")

pdf("../results/figures/discipline_impact.pdf")
p
dev.off()

# Impact*topic: 
impact_dat %>%
  unnest_tokens(topic, topic, token = stringr::str_split, 
                pattern = ", ") %>%
  group_by(impacts, topic) %>% 
  count() %>%
  arrange(impacts, desc(n)) %>% 
  filter(n > 25) %>%
  ggplot(aes(x = impacts, y = n, fill = topic)) +
  geom_col(position = "fill") + 
  scale_fill_brewer(type = "qual", palette = "Set3") + 
  theme_few() + 
  theme(axis.title.x = element_blank()) +
  labs(y = "Proportion", fill = "Topic")
