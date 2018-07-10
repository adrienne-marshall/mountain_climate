# Check arbitration; select only one version of each paper.
# This should eventually be done with combined datasets from round 1 and round 2.

library(tidyverse)

## First, check title and ID agreement on non-arbitrated data: ----------------

# Get data. 
dat <- read_csv("../results/tabular/round2_raw.csv")

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
  mutate(case = 0)


# Identify disagreements.
arb_dat$case[1] <- 1
for(i in 2:nrow(arb_dat)){
  if(arb_dat$paper_id[i] != arb_dat$paper_id[i-1]){
    arb_dat$case[i] <- 1
  } else {
    arb_dat$case[i] <- arb_dat$case[i-1] + 1
  }
  
arb_dat <- arb_dat %>% mutate(case = paste0("c", case))

} #end looping through data frame.

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

write_csv(arb_dat, "../results/tabular/round2_for_arbitration.csv")


# Do we agree on inclusion? --------------
inclusion_errors <- dat %>% 
  mutate(inclusion_IAM = tolower(inclusion_IAM)) %>%
  distinct(inclusion_IAM, paper_id) %>%
  group_by(paper_id) %>%
  count() %>%
  filter(n > 1)

# Look at these. 
dat %>% filter(paper_id %in% inclusion_errors$paper_id) %>% 
  dplyr::select(title, paper_id, inclusion_IAM) # %>% View()

# Great; nothing there. (I did fix some small disagreements in the doc).

# Filter to keepers. 
dat <- dat %>%
  filter(inclusion_IAM %in% c("impacts", "adaptation", "mitigation"))

# Check agreement on impact type. -----------------
impacts_errors <- dat %>% 
  distinct(impacts, paper_id) %>% 
  group_by(paper_id) %>%
  count() %>%
  filter(n > 1)

dat %>%
  filter(inclusion_IAM == "impacts") %>%
  filter(paper_id %in% impacts_errors$paper_id) %>% 
  dplyr::select(title, paper_id, impacts) #%>% View()

# Check agreement on spatial extent. ---------------
# First, extract to new extents. 
# Gaaagh, the new extents have a mismatch with the old ones. 

pat_rep <- tribble(
  ~patterns, ~replacements,
  "larger than 40,000km2 but", "40,000 km2 - PNW",
  "25,000 km2 - 40,000", "10,000 - 40,000 km2",
  "western us", "western us",
  "1-900", "point or plot",
  ">10km2", "100 - 10,000 km2",
  ">1km2", "1 - 100 km2",
  "100 km2 - 500 km2", "100 - 10,000 km2"
)


extent_errors <- dat %>%
  distinct(extent, paper_id) %>%
  group_by(paper_id) %>%
  count() %>%
  filter(n > 1)

dat %>%
  filter(paper_id %in% extent_errors$paper_id) %>%
  dplyr::select(title, paper_id, extent) %>% View()

# Check HUC6 ----------------
huc6_errors <- dat %>% 
  distinct(huc6, paper_id) %>% 
  group_by(paper_id) %>%
  count() %>%
  filter(n > 1)

dat %>% filter(paper_id %in% huc6_errors$paper_id) %>%
  dplyr::select(title, paper_id, huc6) #%>% View()

# Check agreement on biome. ----------------
dat$biome[dat$biome == "alpine/tundra"] <- "alpine tundra"
biome_errors <- dat %>% 
  distinct(biome, paper_id) %>%
  group_by(paper_id) %>%
  count() %>% filter(n > 1)

dat %>% filter(paper_id %in% biome_errors$paper_id) %>%
  dplyr::select(title, paper_id, biome) # %>% View()

# Check topical agreement. ----------
topic_errors <- dat %>%
  distinct(topic, paper_id) %>%
  group_by(paper_id) %>%
  count() %>% filter(n > 1)

dat %>% filter(paper_id %in% topic_errors$paper_id) %>%
  dplyr::select(title, paper_id, topic) # %>% View()

# Only one was left; updated it. 

# Check discipline agreement. ------------
discipline_errors <- dat %>%
  distinct(discipline, paper_id) %>%
  group_by(paper_id) %>%
  count() %>% filter(n > 1)

dat %>% filter(paper_id %in% discipline_errors$paper_id) %>%
  dplyr::select(title, paper_id, discipline) # %>% View()

# Only two disagreed; I updated them. 

# When arbitration is satisfactory, make a data frame with only one version. 
# NOTE - this is currently decent for everything except for spatial extent (April 13, 2018).
dat %>% 
  group_by(paper_id) %>% 
  slice(1) %>% 
  write_csv("../results/tabular/single_copy_results.csv")
