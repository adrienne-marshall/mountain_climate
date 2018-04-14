# Get content analyses and rename columns so they're sane. Could set this up as a function.
library(tidyverse)

# Round 1: -----------------
dat1 <- read_csv("../results/tabular/round1_arbitrated.csv")

patterns <- c("Name", "Title", "Paper ID", "this paper in the Columbia",
              "If this paper addresses", "What is the spatial resolution", "What is the spatial extent", 
              "Where was the study conducted", "In what biome", "what HUC 6 watersheds",
              "type of data", "What discipline", "What topics", "concerns or uncertainty",
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

