# Check arbitration; select only one version of each paper.
# This should eventually be done with combined datasets from round 1 and round 2.

library(tidyverse)
library(lubridate)

## First, check title and ID agreement on non-arbitrated data: ----------------

# Get data. 
dat <- read_csv("../results/tabular/round2_raw.csv")

## Temporary: only keep data from July 11 or later. 
# Will manually copy this into the arbitration sheet. 
# dat <- dat %>% 
#   mutate(Timestamp = mdy_hms(Timestamp)) %>%
#   filter(Timestamp > ymd_hms("2018-07-10 00:00:00"))
#   

# Make everything tolower.
dat <- dat %>% mutate_if(is.character, tolower)

# Rename title and ID.
names(dat)[c(2, 3, 4)] <- c("name", "title", "paper_id") 

# First, check title/id agreement. -----------
# Make everything tolower; remove periods at end.
title_id <- dat %>% 
  mutate(title = gsub('^\\.|\\.$', '', title)) %>%
  mutate(title = gsub('^\\n', '', title)) %>%
  distinct(paper_id, title) # 595 papers. 

# Get cases where one ID has multiple titles associated with it. 
mult_ids <- title_id %>%
  group_by(paper_id) %>% 
  count() %>%
  filter(n > 1)

# Look at the cases where there are multiple titles for one ID.
# Manually change them in the google sheet if so. 
# For those where the title doesn't match, find and fix the title or ID. 
title_id %>% 
  filter(paper_id %in% mult_ids$paper_id) %>% 
  arrange(paper_id) # %>% View() # Should be no data. 


# See if we can identify duplicate titles with fuzzyjoin. 
# Would want to do this with all titles later. 
# get first round of titles to include. 
first_round <- read_csv("../results/tabular/round1_arbitrated_renamed.csv") 

require(fuzzyjoin)
titles1 <- bind_rows(first_round %>% dplyr::select(title, paper_id) %>%
                       mutate(paper_id = as.character(paper_id)),
                     dat %>% dplyr::select(title, paper_id) %>%
                       mutate(paper_id = as.character(paper_id))) %>%
  distinct(title, paper_id) 
titles2 <- titles1 %>%
  rename(id2 = paper_id)

possible_duplicates <- stringdist_inner_join(titles1, 
                                             titles2, 
                                             by = "title",
                                             max_dist = 1,
                                             ignore_case = F) %>%
  mutate(exact_match = ifelse(paper_id == id2, "yes", "no")) %>% 
  filter(exact_match == "no") %>% 
  distinct(paper_id, id2, .keep_all = T) %>%
  arrange(title.x) 
write_csv(possible_duplicates, "../results/tabular/possible_duplicates.csv")
# Manually remove duplicates from arbitration sheet. 


# Was everything reviewed twice? 
# Ignore IDs less than 283 - should be from first round. 
dat %>%
  group_by(paper_id) %>%
  count() %>%
  filter(n < 2) %>% 
  left_join(dat %>% 
              dplyr::select(paper_id, name, title)) # %>% View() # should be no data. 

# If both reviewers agree on exclusion, leave the paper out of the dataset. 
names(dat)[5] <- "inclusion"
dat_exclude <- dat %>%
  dplyr::select(paper_id, title, inclusion) %>% 
  mutate(answer = ifelse(inclusion %in% c("impacts", "adaptation", "mitigation"),
                         "keep", "discard")) %>% 
  group_by(paper_id, answer) %>% 
  count() %>%
  filter(answer == "discard", n >= 2) %>% 
  distinct(paper_id)

# Start making a data set that will be used for arbitration.
arb_dat <- dat %>% 
  filter(!paper_id %in% dat_exclude$paper_id)

# For each variable to be arbitrated, make a list of papers that disagree. 
names(arb_dat)[6:14] <- c("future", "historic", "biome", "extent", "huc6", "location", "data_type", "discipline", "topic")
names(arb_dat)[18] <- "new_data"
arb_dat <- arb_dat %>%
  dplyr::select(-data_type) %>%
  mutate(case = 0) %>%
  arrange(paper_id)


# Identify disagreements.
arb_dat$case[1] <- 1
for(i in 2:nrow(arb_dat)){
  if(arb_dat$paper_id[i] != arb_dat$paper_id[i-1]){
    arb_dat$case[i] <- 1
  } else {
    arb_dat$case[i] <- arb_dat$case[i-1] + 1
  }
} #end looping through data frame.
  
arb_dat <- arb_dat %>% mutate(case = paste0("c", case))

# Identify disagreements for each variable.
vars <- names(arb_dat)[c(6:13, 17)]
var_disagreements <- purrr::map(vars, function(var){
  temp <- arb_dat[,c("paper_id", "title", "case", var)] %>%
    spread(key = "case", value = var) %>%
    mutate(agree = NA)
  for(i in 1:nrow(temp)){
    vals <- c(temp$c1[i], temp$c2[i], temp$c3[i], temp$c4[i])
    vals <- vals[!is.na(vals)]
    if(length(unique(vals)) > 1){ 
      temp$agree[i] <- "no"
    } else {temp$agree[i] <- "yes"} # end if statement.
  } # end looping through temp.
  ans <- temp %>% 
    dplyr::select(paper_id, title, agree) %>%
    arrange(agree, paper_id)
})

# How does our agreement look overall?
purrr::map_df(var_disagreements, . %>% group_by(agree) %>% count()) %>%
  ungroup() %>%
  mutate(variable = rep(vars, each = 2)) %>% 
  spread(key = agree, value = n)

# Not too bad, we mostly agree more than we disagree... 
# and these are strict standards (must be precise agreement).

# For each variable, write a .csv.
purrr::map2(vars, var_disagreements, function(x, y){
  write_csv(y, paste0("../results/disagreement_data/", x, ".csv"))
})

# Write arbitration data to to csv. 
arb_dat <- arb_dat %>% dplyr::select(-case)

write_csv(arb_dat, "../results/tabular/round2_for_arbitration_special.csv")

## For personal use: combine future/historic/type of data.
mine <- var_disagreements[c(1, 2, 9)] %>%
  reduce(full_join, by = c("paper_id", "title")) %>%
  filter(agree.x == "no" | agree.y == "no" | agree == "no") %>%
  dplyr::select(-title)
names(mine)[2:4] <- vars[c(1, 2, 9)]

write_csv(mine, "../results/tabular/adriennes_arbitration.csv")

