# Get content analyses and rename columns so they're sane. Could set this up as a function.
library(tidyverse)

# Round 1: -----------------
dat1 <- read_csv("../results/tabular/round1_arbitrated.csv")

patterns <- c("Name", "Title", "Paper ID", "this paper in the Columbia",
              "If this paper addresses", "What is the spatial resolution", "What is the spatial extent", 
              "Where was the study conducted", "In what biome", "what HUC 6 watersheds",
              "type of data", "What discipline", "What topic", "concerns or uncertainty",
              "related to our research", "related to research in mountainous")

replacements <- c("reviewer", "title", "paper_id", "inclusion_IAM", "impacts", "resolution",
                  "extent", "location", "biome", "huc6", "data_type", "discipline", "topic", 
                  "concerns", "notes1", "notes2")

for(i in 1:length(patterns)){
  names(dat1)[grep(patterns[i], names(dat1))] <- replacements[i]
}

dat1 <- dat1[, replacements]

write_csv(dat1, "../results/tabular/round1_arbitrated_renamed.csv")


# Round 2: ------------------
dat2 <- read_csv("../results/tabular/round2_arbitrated.csv") %>% 
  dplyr::select(-Timestamp) %>% 
  rename(Name = name)

# Change patterns and replacements selectively 
# (order doesn't need to be same as columns; needs to be same as replacements.)

patterns2 <- patterns
patterns2[5] <- "study make a quantitative estimate of"
patterns2[8] <- "Enter place name and GPS"
patterns2[str_which(patterns2, "HUC 6")] <- "Which HUC 6 watershed"
patterns2 <- c(patterns2, "identify an environmental change or trend")
patterns2 <- c(patterns2, "new data collected")

# Make replacements that match patterns2. 
replacements2 <- replacements
replacements2[5] <- "projected"
replacements2 <- c(replacements2, "observed", "new_data")

data.frame(pattern = patterns2, replacement = replacements2)

for(i in 1:length(patterns2)){
  names(dat2)[grep(patterns2[i], names(dat2))] <- replacements2[i]
}

write_csv(dat2, "../results/tabular/round2_arbitrated_renamed.csv")

# Make Rounds 1 and 2 match each other: -----------------
# (reorganize the type of impact).
dat1 <- dat1 %>% 
  mutate(projected = ifelse(str_detect(impacts, "Projected"), "yes", "no")) %>% 
  mutate(observed = ifelse(str_detect(impacts, "Observed"), "yes", "no")) %>% 
  dplyr::select(-impacts)

# (reorganize type of data).
dat1 <- dat1 %>% 
  mutate(new_data = ifelse(str_detect(data_type, "Point data collected specifically|Primary social data"),
                           "yes", "no")) %>%
  dplyr::select(-data_type)

# We didn't look at resolution in the second round - take it out.
dat1 <- dat1 %>% dplyr::select(-resolution)

# Combine the two data frames. 
dat2 <- dat2 %>% mutate(paper_id = as.character(paper_id))

dat <- bind_rows(dat1, dat2)

write_csv(dat, "../results/tabular/all_arbitrated_renamed.csv")





