# Unite both rounds of data collection. 
library(tidyverse)
library(tidytext)
library(widyr)
library(googlesheets)

# Get all data.
dat <- read_csv("../results/tabular/all_arbitrated_renamed.csv")

r1 <- read_csv("../results/tabular/round1_arbitrated_renamed.csv")
r2 <- read_csv("../results/tabular/round2_arbitrated_renamed.csv")

# Inclusion_IAM-----------
dat <- dat %>% 
  mutate(inclusion_IAM = tolower(inclusion_IAM))

# Subset to included papers only. ---------
dat <- dat %>% filter(inclusion_IAM %in% c("impacts", "adaptation", "mitigation"))

# Extent--------

# Make changes semi-manually.
dat <- dat %>% 
  mutate(extent = tolower(extent)) %>% 
  mutate(extent = case_when(str_detect(extent, "point or plot") ~ "point",
                            str_detect(extent, "1-900") ~ "point",
                            str_detect(extent, "1-100 km2") ~ "1 - 100 km2",
                            str_detect(extent, ">10km2") ~ "1 - 100 km2",
                            str_detect(extent, "900m2 - 1km") ~ "1 - 100 km2",
                            str_detect(extent, ">1km2 - 10km2") ~ "1 - 100 km2",
                            str_detect(extent, "100 km2 - 1,500 km2") ~ "100 - 1500 km2",
                            str_detect(extent, "100 km2 - 500 km2") ~ "100 - 1500 km2",
                            str_detect(extent, "500 km2 - 1500 km2") ~ "100 - 1500 km2",
                            str_detect(extent, "1,500 km2 - 25,000 km2") ~ "1500 - 25000 km2",
                            str_detect(extent, "1500 km2 - 25000 km2") ~ "1500 - 25000 km2",
                            str_detect(extent, "25,000 km2 - 40,000") ~ "25000 - 40000 km2",
                            str_detect(extent, "larger than 40") ~ "40000 km2 - PNW", # order is important here. 
                            str_detect(extent, "pacific northwest") ~ "pacific northwest",
                            str_detect(extent, "western us") ~ "western us",
                            str_detect(extent, "continental") ~ "western us",
                            TRUE ~ extent)) %>% 
  mutate(extent = str_replace(extent, "pnw", "PNW")) 

# Fix those that need it. 
dat$extent[dat$paper_id == "209"] <- "western us"

# Location-----------

# Get Courtney's locations:
loc <- read_csv("../results/tabular/location2_CC.csv") %>% 
  distinct(paper_id, location) %>% 
  rename(cc_location = location) %>% 
  mutate(paper_id = as.character(paper_id))

# Replace data locations with Courtney's. 
dat <- left_join(dat, loc, by = "paper_id") %>% 
  mutate(location = ifelse(!is.na(cc_location), cc_location, location)) %>% 
  dplyr::select(-cc_location)

# Fix a few specific problems.
dat$location[dat$paper_id == "692"] <- "50.6666,-120.333"
dat$location[dat$paper_id == "307"] <- "47.2447,-121.0666"
dat$location[dat$paper_id == "657"] <- "43.2058067,-113.5001702"


# Biome-----------
dat <- dat %>% mutate(biome = tolower(biome))
dat <- dat %>% 
  mutate(biome = case_when(is.na(biome) ~ "n/a (not biome specific or all biomes - see codebook)",
                           TRUE ~ biome))

dat <- dat %>% 
  unnest_tokens(biome, biome, token = str_split, pattern = ", ") %>% 
  mutate(biome = case_when(biome == "alpine/tundra" ~ "alpine tundra",
                           str_detect(biome, "boreal") ~ "forest: temperate",
                           str_detect(biome, "n/a") ~ "N/A",
                           TRUE ~ biome)) %>% 
  # distinct(paper_id, biome, .keep_all = T) %>% 
  nest(biome) %>% 
  mutate(biome = purrr::map(data, unlist)) %>% 
  mutate(biome = map_chr(biome, paste, collapse = ", ")) %>% 
  dplyr::select(-data)


# Huc6------------
# Make all numeric. 
dat <- dat %>% 
  unnest_tokens(huc6, huc6, token = str_split, pattern = ", ") %>% 
  mutate(huc6 = gsub("\\s*\\([^\\)]+\\)", "", huc6)) %>% # get rid of parentheses. 
  filter(huc6 != "both us and canada") %>% # got separated by comma
  mutate(huc6 = ifelse(str_detect(huc6, "[[:digit:]]"), 
                       str_replace(huc6, "\\D.*", ""),
                       huc6)) %>% # extract digits only.
  filter(!str_detect(huc6, "dry cr. exp.")) %>%
  mutate(huc6 = case_when(str_detect(huc6, "all in crb u") ~ "0101, 0102, 0103, 0200, 0300, 0401, 0402, 0501, 0502, 0601, 0602, 0603, 0701, 0702, 0703, 0800, 0900, 1002",
                          str_detect(huc6, "all crb") ~ "0101, 0102, 0103, 0200, 0300, 0401, 0402, 0501, 0502, 0601, 0602, 0603, 0701, 0702, 0703, 0800, 0900, 1002, kooteney, okanagan, columbia",
                          str_detect(huc6, "all in crb canada") ~ "kooteney, okanagan, columbia",
                          str_detect(huc6, "all washington") ~ "0102, 0103, 0200, 0300, 0601, 0701, 0800",
                          str_detect(huc6, "all idaho") ~ "0101, 0102, 0103, 0603, 0601, 0602, 0502, 0501, 0401, 0402",
                          str_detect(huc6, "all oregon") ~ "0601, 0701, 0502, 0702, 0501, 0200, 0703, 0800, 0900, 1002",
                          str_detect(huc6, "in the rocky") ~ "kooteney, okanagan, columbia, 0101, 0102, 0103, 0604, 0602, 0401, 0402",
                          str_detect(huc6, "anadromous") ~ "0800, 0900, 0701, 0702, 0703, 0200, 0300, 0601, 0602",
                          str_detect(huc6, "wyoming") ~ "0401",
                          TRUE ~ huc6)) %>% 
  nest(huc6) %>% 
  mutate(huc6 = purrr::map(data, unlist)) %>% 
  mutate(huc6 = map_chr(huc6, paste, collapse = ", ")) %>% 
  dplyr::select(-data)

# Discipline------------
# Just make lower case - remove/join a few
# Get rid of geography - replace.
dat <- dat %>% 
  mutate(discipline = tolower(discipline)) %>% 
  mutate(discipline = case_when(paper_id %in% c("81", "163", "209", "466", "626", "651", "733", "797") ~ "ecology",
                                paper_id == "181" ~ "ecology, hydrology",
                                paper_id %in% c("193", "224", "471") ~ "hydrology",
                                paper_id == "425" ~ "ecology, policy or management",
                                paper_id == "439" ~ "meteorology and climatology",
                                TRUE ~ discipline)) %>% 
  unnest_tokens(discipline, discipline, token = stringr::str_split, pattern = ", ") %>% 
  mutate(discipline = str_replace(discipline, "pedology|geology", "geology/pedology")) %>%
  # mutate(discipline = str_replace(discipline, "biology|ecology", "biology/ecology")) %>%
  mutate(discipline = case_when(discipline %in% c("biogeoscience", "biogeochemistry") ~ "ecology",
                                discipline == "management" ~ "policy or management",
                                discipline == "law" ~ "policy or management",
                                # discipline == "glaciology" ~ "hydrology",
                                TRUE ~ discipline)) %>% 
  nest(discipline) %>%
  mutate(discipline = purrr::map(data, unlist), 
         discipline = map_chr(discipline, paste, collapse = ", ")) %>% 
  dplyr::select(-data)

# Topics---------
# dat %>% 
#   mutate(topic = gsub("\\s*\\([^\\)]+\\)", "", topic)) %>% 
#   unnest_tokens(topic, topic, token = str_split, pattern = ",") %>% 
#   unnest_tokens(topic, topic, token = str_split, pattern = ";") %>%
#   mutate(topic = str_trim(topic)) %>% 
#   group_by(topic) %>% 
#   count(sort = T) %>% 
#   filter(topic != "") %>% 
#   mutate(n = ceiling(n/2)) %>% 
#   write_csv("../results/tabular/topics_used.csv")

# Get papers that have river geomorphology and restoration as a topic. 
rest <- dat %>% 
  filter(str_detect(topic, "geomorphology and restoration")) %>% 
  dplyr::select(title, paper_id, topic) %>% 
  distinct(paper_id, .keep_all = T) 
rest <- rest %>% 
  mutate(reviewer = rep(c("AM", "MF", "PE"), each = ceiling(nrow(rest)/3))[1:nrow(rest)])
write_csv(rest, "../results/disagreement_data/restoration_papers.csv")



# Replace uncommon topics. 
topic_df <- "https://docs.google.com/spreadsheets/d/19KaYfyF_iVavOqKDFHs08TPBodRPclueOXtFlJLu9q0/edit#gid=183943559" %>% 
  gs_url() %>% 
  gs_read()
names(topic_df)[str_detect(names(topic_df), "CC arbitration")] <- "new_topic"
# names(topic_df)[str_detect(names(topic_df), "Extreme Consolidation")] <- "consolidated_topic"
names(topic_df)[str_detect(names(topic_df), "Topic coded")] <- "topic"

topic_df <- topic_df %>% 
  dplyr::select(topic, new_topic) %>% 
  mutate(new_topic = ifelse(new_topic %in% c("carbon cycle", "carbon sequestration", 
                                             "carbon stocks", "carbon emissions"), 
                            "carbon cycle", new_topic))

# Save final topics. 
# binned_topics <- gs_new(title = "final_binned_topics", input = topic_df)

write_csv(topic_df, "../results/tabular/final_topic_binning.csv")

dat <- dat %>% 
  mutate(topic = gsub("\\s*\\([^\\)]+\\)", "", topic)) %>% 
  unnest_tokens(topic, topic, token = str_split, pattern = ",") %>% 
  unnest_tokens(topic, topic, token = str_split, pattern = ";") %>%
  mutate(topic = str_trim(topic)) %>% 
  left_join(topic_df) %>% 
  mutate(topic = new_topic) %>% 
  filter(topic != "N/A") %>% 
  dplyr::select(-new_topic) %>% 
  nest(topic) %>%
  mutate(topic = purrr::map(data, unlist), 
         topic = map_chr(topic, paste, collapse = ", ")) %>% 
  dplyr::select(-data)

# Manual changes to topic: 
# Paper # 67 shouldn't have "glaciers" in the topic. 
dat$topic[dat$paper_id == "67"] <- str_replace(dat$topic[dat$paper_id == "67"], "glaciers, ", "")
dat$topic <- str_replace(dat$topic, "land use/ land cover", "land use/land cover")

# Projected and observed -----------
# Manual fix: 
dat$observed[dat$paper_id == "266"] <- "no"
dat$projected[dat$paper_id == "266"] <- "yes"

# New data------------
# nothing to change.

# Write a version with two copies of everything. 
write_csv(dat, "../results/tabular/clean_data_two_copies.csv")

# Make just one version of each paper.
dat <- dat %>% group_by(paper_id) %>% slice(1) %>% ungroup()

write_csv(dat, "../results/tabular/all_dat_cleaned.csv")

# Write to googlesheets.---------
# gs_dat <- gs_ls()
# 
# # If the sheet is already written: 
# if(sum(str_detect(gs_dat$sheet_title, "all_dat_cleaned")) > 0){
#   gs_key1 <- gs_key(gs_dat$sheet_key[gs_dat$sheet_title == "all_dat_cleaned"])
#   gs_edit_cells(ss = gs_key1, input = dat, anchor = "A1")
#   
# } else { # If not already written. 
#   gs_dat <- gs_new(title = "all_dat_cleaned", 
#                    input = dat)
# }






