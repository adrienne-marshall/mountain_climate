# Make radar plots of spatial extent by topic and discipline. 
library(tidyverse)
library(tidytext)
library(ggthemes)
library(pals)
source("coord_radar.R")

# Get data. 
# These are the topics that Micah and Meghan cleaned and Courtney arbitrated. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Calculate IAM counts for percentages. 
extent_counts <- dat %>% 
  group_by(extent) %>% 
  count() %>% 
  rename(extent_n = n)

# Make a data frame of counts by discipline. 
disc_counts <- dat %>% 
  unnest_tokens(discipline, discipline, token = str_split, pattern = ", ") %>% 
  distinct(paper_id, discipline, .keep_all = T) %>% 
  group_by(extent, discipline) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(extent_counts, by = "extent") %>% 
  mutate(percent = 100*n/extent_n) %>% 
  mutate(discipline = ifelse(str_detect(discipline, "environmental chem"), 
                             "environmental chemistry", discipline)) %>% 
  mutate(discipline = str_replace(discipline, "/| and | or ", "/\n")) %>% 
  mutate(discipline = str_replace(discipline, " ", "\n"))

# Expand to include all possible combinations of topic/IAM.
all_disc <- expand.grid(discipline = unique(disc_counts$discipline),
                        extent = unique(disc_counts$extent))
disc_counts <- disc_counts %>% full_join(all_disc) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         percent = ifelse(is.na(percent), 0, percent))

# Make discipline plot.
disc_plot <- ggplot(disc_counts,
                    aes(x = discipline, y = percent, 
                        color = extent, group = extent)) + 
  geom_point(size = 1.2) + 
  geom_line(size = 0.5) + 
  coord_radar() + 
  # scale_y_continuous(trans = "log", breaks = c(0, 1, 10, 100, 200)) +  # for number. 
  scale_color_manual(values = pals::brewer.paired(8), guide = F) + # because same as for topic.  
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.8),
        axis.text.x = element_text(size = 10)) + 
  labs(y = "Percent of papers in category",
       title = "(a)")
disc_plot

# Now topics ----------------
# Make a vector of topics to include.
# This is currently just topics that occur more than n times. 
topics_include <- dat %>% 
  unnest_tokens(topic, topic, token = str_split, pattern = ", ") %>% 
  group_by(topic) %>% 
  count(sort = T) %>% 
  filter(!topic %in% c("other", "na", "temperature", "precipitation", "mathematics")) %>% 
  ungroup() %>% 
  top_n(20)

# Make a data frame of counts by topic. 
topic_counts <- dat %>% 
  unnest_tokens(topic, topic, token = str_split, pattern = ", ") %>% 
  distinct(paper_id, topic, .keep_all = T) %>% 
  group_by(extent, topic) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(extent_counts, by = "extent") %>% 
  mutate(percent = 100*n/extent_n) %>% 
  filter(topic %in% topics_include$topic) %>% 
  mutate(topic = str_replace(topic, "/| and | or ", "/\n")) %>% 
  mutate(topic = str_replace(topic, " ", "\n"))

# Expand to include all possible combinations of topic/IAM.
all_topics <- expand.grid(topic = unique(topic_counts$topic),
                          extent = unique(topic_counts$extent))
topic_counts <- topic_counts %>% full_join(all_topics) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         percent = ifelse(is.na(percent), 0, percent))

# Make topic plot. 
topic_plot <- ggplot(topic_counts,
                     aes(x = topic, y = percent, 
                         color = extent, group = extent)) + 
  geom_point(size = 1.2) + 
  geom_line(size = 0.5) + 
  coord_radar() + 
  # scale_y_continuous(trans = "log", breaks = c(0, 1, 10, 100, 200)) +  # for number. 
  scale_color_manual(values = pals::brewer.paired(8)) +   
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9, hjust = 10),
        legend.position = c(-0.05, 0.1)) + 
  labs(y = "Percent of papers in category",
       title = "(b)")
topic_plot

library(patchwork)
p <- disc_plot + topic_plot + plot_layout(nrow = 1)
p

pdf("../results/figures/extent_topic_discipline_radar.pdf",
    width = 10, height = 5)
p
dev.off()
