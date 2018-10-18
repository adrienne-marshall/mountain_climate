# Make bar charts of most common topics, disciplines, and frequency of I/A/M. 

library(tidyverse)
library(tidytext)
library(patchwork)

# Get data. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Topics.
topic_count <- dat %>% 
  unnest_tokens(input = topic, output = topic, token = stringr::str_split, pattern = ", ") %>% 
  mutate(topic = str_trim(topic)) %>% 
  group_by(topic) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  filter(!topic %in% c("mathematics", "na", "other")) %>% 
  top_n(30) # arbitrary

topic_plot <- ggplot(topic_count, aes(x = fct_reorder(topic, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Number of papers",
       title = "(c)") + 
  scale_y_continuous(expand = c(0, 0.9)) + 
  theme_minimal()
topic_plot

# Disciplines
discipline_count <- dat %>% 
  unnest_tokens(discipline, discipline, token = stringr::str_split, pattern = ", ") %>% 
  mutate(discipline = str_trim(discipline)) %>% 
  group_by(discipline) %>% 
  count(sort = T)

discipline_plot <- ggplot(discipline_count, 
                          aes(x = fct_reorder(discipline, n), y = n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Number of papers",
       title = "(b)") + 
  scale_y_continuous(expand = c(0, 0.9)) + 
  theme_minimal()
discipline_plot

# I/A/M plot
iam_count <- dat %>% 
  mutate(IAM = case_when(
    inclusion_IAM == "impacts" & projected == "yes" & observed == "yes" ~ "Projected impacts, Observed impacts",
    inclusion_IAM == "impacts" & projected == "no" & observed == "no" ~ "Implications",
    inclusion_IAM == "impacts" & projected == "yes" & observed == "no" ~ "Projected impacts",
    inclusion_IAM == "impacts" & projected == "no" & observed == "yes" ~ "Observed impacts",
    inclusion_IAM == "mitigation" ~ "Mitigation",
    inclusion_IAM == "adaptation" ~ "Adaptation")) %>% 
  unnest_tokens(IAM, IAM, token = stringr::str_split, pattern = ", ",
                to_lower = F) %>% 
  group_by(IAM) %>% 
  count(sort = T)

iam_plot <- ggplot(iam_count,
                   aes(x = fct_reorder(IAM, n), y = n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Number of papers",
       title = "(a)") + 
  scale_y_continuous(expand = c(0, 0.9)) + 
  theme_minimal()
iam_plot

# Make and save composite figure. 
p <- iam_plot + discipline_plot + topic_plot

pdf("../results/figures/iam_topic_discipline_bar.pdf",
    width = 10, height = 6)
p
dev.off()
  
