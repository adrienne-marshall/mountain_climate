# Check arbitration; select only one version of each paper.
# This should be done with combined datasets from round 1 and round 2 (?)

library(tidyverse)

# Get data. 
dat <- read_csv("../results/tabular/round1_arbitrated_renamed.csv")

# Get rid of weird column with repeat of headers.
dat <- dat[!grepl("Is this paper", dat$inclusion_IAM), ]

# Make everything tolower.
dat <- dat %>% mutate_if(is.character, tolower)

# Look at agreement (for each column?)

# First, check title/id agreement. -----------
# Make everything tolower; remove periods at end.
title_id <- dat %>% 
  mutate(title = gsub('^\\.|\\.$', '', title)) %>%
  mutate(title = gsub('^\\n', '', title)) %>%
  distinct(paper_id, title)

mult_ids <- title_id %>%
  group_by(paper_id) %>% 
  count() %>%
  filter(n > 1)

# Do a visual check to see if the titles approximately match. 
title_id %>% filter(paper_id %in% mult_ids$paper_id) #%>% View()

# These are all good; change them to match exactly.
title_id <- title_id %>% group_by(paper_id) %>% slice(1)

for(i in 1:nrow(title_id)){
  dat$title[dat$paper_id == title_id$paper_id[i]] <- title_id$title[i]
}

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
