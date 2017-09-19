library(tidyverse)

dat <- read_csv("data/aggregated_data_Sept8.csv")

dat$include_final[is.na(dat$include_final)] <- "Maybe"

#Incorporate final answer.
dat <- dat %>% 
  mutate(final_answer = ifelse(include_final == "Maybe",`Include?`, include_final))

#How many papers do we have?
dat %>% group_by(final_answer) %>% count()

#Clean up and write a .csv.
final_dat <- dat %>%
  filter(final_answer == "Yes") %>%
  dplyr::select(id:source)

write_csv(final_dat, "data/final_corpus.csv")
