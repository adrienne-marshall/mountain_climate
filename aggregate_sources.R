#Get all sources with yeses, put them together. 
library(tidyverse)

d1 <- read_csv("data/wos_cab_pro_0606.csv")
d2 <- read_csv("data/crossref_0711.csv")

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

#Get crossref results for these data so the format is consistent. ---------
require(rcrossref)

#First, everything with a DOI: 
doi_list <- dat$doi[!is.na(dat$doi)]

crossref_results <- vector("list", length(doi_list))
for(i in 1:length(crossref_results)){
  try({
  crossref_results[[i]] <- cr_works(dois = doi_list[i])
  })
  print(i)
}

crossref_results2 <- lapply(crossref_results, function(x){x[[2]]})
crossref_df <- bind_rows(crossref_results2)

#6 errors: 
dois_needed <- c("10.1043/0044-7447(2001)030(0410:MGAMSO)2.0.CO;2",
                 "10.1175/1520-0442(2003)016(1912:HOTWUS)2.0.CO;2",
                 "10.1175/1520-0442(2003)016(0799:SPDOTN)2.0.CO;2",
                 "10.1175/1520-0442(2002)015&lt;1926:IOIACO&gt;2.0.CO;2",
                 "10.1175/1520-0442(2003)016&lt;1912:HOTWUS&gt;2.0.CO;2",
                 "10.1002/(SICI)1099-1085(199609)10:9&lt;1173::AID-HYP368&gt;3.0.CO;2-V")

titles_needed <- dat$title[is.na(dat$doi)]

#Get the titles that weren't associated with DOIs.
title_search_results <- vector("list", length(titles_needed))
for(i in 1:length(title_search_results)){
  try({
    title_search_results[[i]] <- cr_works(flq = (query.title = titles_needed[i]))
    print(i)
  })
}
  
messages <- lapply(title_search_results, function(x){x$message})
messages <- unlist(messages)

#Searching for the titles that didn't have DOIs didn't work...

#Make a data frame that looks like M (from bibliometrics.R):