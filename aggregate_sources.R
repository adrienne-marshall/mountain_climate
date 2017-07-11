#Get all sources with yeses, put them together. 
library(tidyverse)

d1 <- read_csv("data/wos_cab_pro_0606.csv")
d2 <- read_csv("data/crossref_0606.csv")

names(d1) <- gsub(" ", "_", names(d1))
names(d1) <- gsub(":", ".", names(d1))
names(d2) <- gsub(" ", "_", names(d2))

#Try to figure out what's happening with reviewers. 
x <- d1 %>% dplyr::select(doi, reviewer, New_reviewer._No_international) 
names(x)[3] <- "new_reviewer"
x %>% group_by(reviewer, new_reviewer) %>% summarise(n = n()) %>% View()
#this makes it seem like the right number of papers got reviewed...

#crossref: 
y <- d2 %>% dplyr::select(reviewer, New_reviewer) %>%
  group_by(reviewer, New_reviewer) %>%
  count()
#Ok, with crossref, it seems like Becky and Paris's didn't get reviewed - need to do that. 

# For now, make a dataset with all papers that have "yes" to include.

inclusions <- c("Yes", "Maybe", NA)

#WOS: 1616 papers, 456 for inclusion (keeping maybes and NAs)
wos_include <- d1 %>% 
  filter(source == "web_of_science") %>%
  filter(include %in% inclusions) %>%
  filter(!is.na(title))

#Proquest: 790 papers, 64 getting included with maybes and NAs.
pro <- d1 %>%
  filter(source == "proquest") %>%
  filter(`Include?` %in%  inclusions)

#Cabdirect: 1172 papers, 339 included. 
cab <- d1 %>%
  filter(source == "cabdirect") %>%
  filter(`Include?`%in% inclusions)

#Crossref: #598 papers, 90 included.  
cross <- d2 %>% 
  filter(`Include?` %in% inclusions)

#Aggregate data. ---------
woscabpro <- bind_rows(wos_include, pro, cab) %>%
  dplyr::select(id, doi, authors, title, journal, abstract, year, disciplines, type, keywords, source, include, `Include?`) %>%
  filter(!is.na(title)) %>%
  mutate(include_final = ifelse(source == "web_of_science",
                                include, 
                                `Include?`)) %>%
  dplyr::select(-include, -`Include?`)
  

cross <- cross %>%
  mutate(abstract = NA, 
         keywords = NA, 
         source = "crossref") %>%
  dplyr::select(DOI, author, title, container.title, abstract, created, subject, type, keywords, source, `Include?`)

cross <- cross %>%
  mutate(id = (max(woscabpro$id)+1):(max(woscabpro$id)+nrow(cross))) %>%
  dplyr::select(id, everything()) %>%
  mutate(created = as.integer(basename(created)))

names(cross) <- names(woscabpro)
         
dat <- bind_rows(cross, woscabpro) 
names(dat)[names(dat) == "include_final"] <- "include"

write_csv(dat, "data/aggregated_papers.csv")

#Get crossref results for these data so the format is consistent. 
require(rcrossref)

#First, everything with a DOI: 
doi_list <- dat$doi[!is.na(dat$doi)]

crossref_results <- vector("list", length(doi_list))
for(i in 1:length(crossref_results)){
  try({
  crossref_results[[i]] <- cr_works(dat, dois = doi_list[i])
  })
}
  
 