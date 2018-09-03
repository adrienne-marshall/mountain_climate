# This script gets our data from googlesheets. 
library(tidyverse)
library(googlesheets)

# Register findings from round 1 (arbitrated):
round1_arb_url <- "https://docs.google.com/spreadsheets/d/1QrMRc_AcVNoHDRNQsgKLbUgApMVPaoSiHx7X1ZF2tc4/edit?usp=drive_web&ouid=103747957307001540151"
round1_arb_df <- round1_arb_url %>% 
  gs_url() %>% 
  gs_read()
write_csv(round1_arb_df, 
          "../results/tabular/round1_arbitrated.csv")

# Findings from round 1 (not arbitrated):
round1_raw_url <- "https://docs.google.com/spreadsheets/d/19iDaMEICVF0U7N_blI_oR3KoUoJ5xIjBpUNj71aZQWc/edit#gid=1349749660"
round1_raw_df <- round1_raw_url %>% 
  gs_url() %>% 
  gs_read()
write_csv(round1_raw_df, 
          "../results/tabular/round1_raw.csv")

# Second round responses: --------------
round2_url <- "https://docs.google.com/spreadsheets/d/1OWYcXmI4mgJ8d9K2YCtCYzOeeyCZEoQIz51CUrt4zy0/edit#gid=1355036755"
round2_df <- round2_url %>% 
  gs_url() %>% 
  gs_read()
write_csv(round2_df, "../results/tabular/round2_raw.csv")

# Findings from round 2 - arbitrated. 
round2_arb_url <- "https://docs.google.com/spreadsheets/d/1DJkG4KAg0z_92iDTivvUneppHk9sXDFW58Si8Q3B42I/edit#gid=1862807847"
round2_arb_df <- round2_arb_url %>% 
  gs_url() %>% 
  gs_read()
write_csv(round2_arb_df, "../results/tabular/round2_arbitrated.csv")
