# Make a spreadsheet with most prolific authors.
library(tidyverse)
library(tidytext)
library(stringr)

dat <- read_csv("data/final_corpus.csv") %>%
  filter(!is.na(title))

author_df <- dat %>% select(title, authors) %>% 
  unnest_tokens(output = authors_long, input = authors, token = stringr::str_split, pattern = ";")

author_df <- author_df %>% 
  mutate(authors_long = str_trim(authors_long)) %>%
  mutate(last = ifelse(grepl(",", authors_long) == TRUE,
                       str_extract(authors_long, "[^,]*"),
                       str_extract(authors_long, "[^ ]*$"))) %>%
  mutate(first_init = ifelse(grepl(",", authors_long) == TRUE,
                             strsplit(authors_long, " "),
                             str_sub(authors_long, start = 1, end = 1))) %>%
  unnest(first_init) %>%
  group_by(authors_long, title) %>%
  slice(2) %>%
  ungroup() %>%
  arrange(title) %>%
  mutate(first_init = str_sub(first_init, 1, 1)) %>%
  mutate(author = paste0(first_init, ". ", last)) %>%
  select(-last, -first_init)

counts <- author_df %>% 
  group_by(author) %>%
  count(sort = T)

counts %>% filter(n>1) %>% nrow()
# If we want more authors with more than one paper, we'll have 494 authors.

counts %>% filter(n>2) %>% nrow()
# If we want authors with more than two papers, we'll have 244 authors. 

counts %>% filter(n>3) %>% nrow()
# If we want authors with more than three papers, we'll have 123 authors. 

counts %>% filter(n>4) %>% nrow()
# And if we want authors with more than four papers, we'll have 69 authors. 

# Make a data frame with titles written by each author: 
author_titles <- author_df %>%
  distinct(author, title)

authors <- left_join(counts, author_titles, by = "author") %>%
  distinct(author, n, .keep_all = TRUE)
  
# Make a data frame with other names we have for each author
aliases <- data.frame(author = unique(authors$author),
                      alias1 = NA, alias2 = NA, alias3 = NA, alias4 = NA)
for(i in 1:nrow(aliases)){
  temp <- author_df %>% filter(author == aliases$author[i])
  temp_names <- unique(temp$authors_long)
  
  if(length(temp_names) <= 4){
    temp_names <- c(temp_names, rep("", 4-length(temp_names)))
  } else{temp_names <- temp_names[1:4]}
  
  aliases[i, 2:5] <- temp_names
}

aliases <- aliases %>%
  unite(col = "aliases", alias1, alias2, alias3, alias4, sep = "; ")

authors <- left_join(authors, aliases)

top_authors <- authors %>% filter(n > 1)

write.csv(top_authors, "data/top_authors.csv")
