#Clean author data and prepare for network analysis. 
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(stringr)
library(widyr)

dat <- read_csv("data/aggregated_data.csv")

#get rid of NA titles. 
dat <- dat %>% filter(!is.na(title))

#make a data frame that lists all the authors in separate columns for each paper.
author_df <- dat %>% select(id, authors) %>% 
  unnest_tokens(output = authors_long, input = authors, token = stringr::str_split, pattern = ";")

#... the big problem is mixed treatment of names (e.g., initials vs full first name).
#try converting all to first initial only. 

author_df <- author_df %>% 
  mutate(authors_long = str_trim(authors_long)) %>%
  mutate(last = ifelse(grepl(",", authors_long) == TRUE,
    str_extract(authors_long, "[^,]*"),
    str_extract(authors_long, "[^ ]*$"))) %>%
  mutate(first_init = ifelse(grepl(",", authors_long) == TRUE,
                             strsplit(authors_long, " "),
                             str_sub(authors_long, start = 1, end = 1))) %>%
  unnest(first_init) %>%
  group_by(authors_long, id) %>%
  slice(2) %>%
  ungroup() %>%
  arrange(id) %>%
  mutate(first_init = str_sub(first_init, 1, 1)) %>%
  mutate(author = paste0(first_init, ". ", last)) %>%
  select(-last, -first_init)

#now column "author" contains the most standard version.
#looks like we have 2144 unique authors. 

#Get pairwise author count:
author_pairs <- author_df %>%
  pairwise_count(author, id, sort = TRUE, upper = FALSE)

#plot network of author collaborations:
#this takes a long time if you don't filter number of collaborations. 
set.seed(1234)
author_pairs%>%
  filter(n > 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()


#join with subject data (using journal, discipline, keyword?). network analysis by topic. 








