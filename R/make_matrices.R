# Make matrices. 
library(tidyverse)
library(tidytext)

dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# First: a matrix with papers in columns, and topics or disciplines in rows. -----------
cases <- c("topic", "discipline")

purrr::map(cases, function(case){
  case_dat <- dat %>% rename(col1 = case) %>% 
    dplyr::select(paper_id, col1) %>% 
    unnest_tokens(col1, col1, token = stringr::str_split, pattern = ", ") %>% 
    filter(col1 != "other") %>% 
    mutate(paper_count = 1)
  
  all_poss <- expand.grid(col1 = unique(case_dat$col1),
                          paper_id = unique(case_dat$paper_id))
  
  dat_wide <- right_join(case_dat, all_poss, by = c("col1", "paper_id")) %>% 
    distinct(paper_id, col1, .keep_all = T) %>% 
    mutate(paper_count = ifelse(is.na(paper_count), 0, paper_count)) %>% 
    spread(paper_id, paper_count)
  
  names(dat_wide)[1] <- case
  
  write_csv(dat_wide, paste0("../results/tabular/matrices/paper_", case, ".csv"))
})

# Now make matrices with HUC, biome, or extent in the rows, and topics in the columns -------------
categories <- c("huc6", "biome", "inclusion_IAM", "extent")
info <- expand.grid(cases = cases, categories = categories)
info <- info %>% mutate(index = 1:nrow(info)) %>% split(.$index)

purrr::map(info, function(info_df){
  case <- as.character(info_df$cases[1])
  category <- as.character(info_df$categories[1])
  
  # If category is HUC6, get rid of large spatial extent papers. 
  if(category == "huc6"){
    case_dat <- dat %>% 
      filter(!extent %in% c("western us", "pacific northwest", "40000 km2 - PNW"))
  } else {case_dat <- dat}
  
  names(case_dat)[names(case_dat) == category] <- "col2"
  names(case_dat)[names(case_dat) == case] <- "col1"
  
  case_dat <- case_dat %>% 
    dplyr::select(paper_id, col1, col2) %>% 
    unnest_tokens(col1, col1, token = stringr::str_split, pattern = ", ") %>% 
    unnest_tokens(col2, col2, token = stringr::str_split, pattern = ", ") %>% 
    filter(col1 != "other") %>% 
    group_by(col1, col2) %>% 
    count() %>% 
    spread(col1, n)
  
  case_dat[is.na(case_dat)] <- 0
  names(case_dat)[1] <- category
  
  write_csv(case_dat, paste0("../results/tabular/matrices/", case, "_", category, ".csv"))
})
