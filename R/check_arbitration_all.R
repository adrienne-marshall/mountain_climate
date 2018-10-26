## Check arbitration with full data set. 
library(tidyverse)

dat <- read_csv("../results/tabular/all_arbitrated_renamed.csv")

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

# Make changes: 
dat$inclusion_IAM[dat$paper_id == "444"] <- "Impacts" #disagreed between round 1 and 2. 

# Filter to keepers. 
dat <- dat %>%
  mutate(inclusion_IAM = tolower(inclusion_IAM)) %>% 
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
  dplyr::select(title, paper_id, inclusion_IAM) #%>% View() # No errors. 

# Check agreement on spatial extent. ---------------
# First, extract to new extents. 


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
  mutate(extent = tolower(extent)) %>% 
  group_by(paper_id) %>%
  count() %>%
  filter(n > 1)

dat %>%
  filter(paper_id %in% extent_errors$paper_id) %>%
  dplyr::select(title, paper_id, extent) %>% 
  arrange(as.numeric(paper_id))
  # write_csv("../results/disagreement_data/extent2.csv")

# Check agreement on location.------------
location_errors <- dat %>% 
  filter(str_detect(location, "[[:digit:]]")) %>% 
  distinct(location, paper_id) %>% 
  group_by(paper_id) %>% 
  count() %>% 
  filter(n > 1)

dat %>% 
  filter(paper_id %in% location_errors$paper_id) %>%
  filter(str_detect(location, "[[:digit:]]")) %>% 
  dplyr::select(title, paper_id, location) %>% 
  arrange(as.numeric(paper_id)) %>% 
  write_csv("../results/disagreement_data/location2.csv")

# Check HUC6 ----------------
huc6_errors <- dat %>% 
  distinct(huc6, paper_id) %>% 
  group_by(paper_id) %>%
  count() %>%
  filter(n > 1)

dat %>% filter(paper_id %in% huc6_errors$paper_id) %>%
  dplyr::select(title, paper_id, huc6) %>% 
  arrange(paper_id) %>% 
  View()

# Most problems here are from multiple rounds. 


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
