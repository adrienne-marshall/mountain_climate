#Try to get full text for articles being included.

#This needs to be done with a campus ip address.

library(tidyverse)
library(crminer)

#get data
dat <- read_csv("data/aggregated_data.csv") %>%
  filter(!is.na(title))
#158 papers are missing DOIs
dat_dois <- dat %>% filter(!is.na(doi))

#Get system environment variable.
Sys.getenv("CROSSREF_TDM")

#data(dois_pensoft) - testing only
#links <- lapply(dois_pensoft[1:3], crm_links, type = "xml")
links <- lapply(dois_pensoft[1:3], crm_links, type = "pdf")
text <- crm_text(url = links[[1]])

#get links for DOIs
links <- lapply(dat_dois$doi, crm_links, type = "xml")

#links are only showing up for elsevier papers.... :(



