#Set working directory.
home <- "/Volumes/research_storage/mountain_climate"

#Load libraries
library(tidyverse)

#Read recs with out comments.
wos <- read.csv("data/wos_recs.csv")
wos[wos==""] <- "NA"


#For this script - merging - keep only identifying information and round 4 answers. 
wos <- wos[,c(1:13, 34)]
wos <- wos[,-8]
names(wos)[13] <- "include"
names(wos)[1] <- "id"

wos <- wos %>% mutate(source = "web_of_science")

#Now get CabDirect. -----
cab1 <- read_csv("data/cabdirect1.csv")
cab2 <- read_csv("data/cabdirect2.csv")
cab <- bind_rows(cab1, cab2)

#This removed about 130 items. 

#Fit cab to approximately same columns as wos. ----
#cab doesn't have disciplines (or wos_id - but it does have an identifier of its own..)
#It does have some other interesting information, but we could look at that later. 
cab2 <- cab %>% 
  dplyr::select(Authors, Title, `Document title`, `Languages of Text`, `Item Types`,
                Descriptors ,`Abstract Text`, `Year of Publication`, Doi)
cab <- cbind(cab2, cab[,1])
cab <- cab %>% mutate(id = NA)
cab <- cab %>% dplyr::select(id, everything()) %>% 
  mutate(include = NA) %>%
  mutate(source = "cabdirect") %>%
  add_column(disciplines = rep(NA, nrow(cab)), .after = 10)

names(cab) <- names(wos)

#Filter cab in some obvious ways. --------
#journal articles, in English. #Starts at 1705 articles. 
cab <- cab %>% 
  filter(type %in% c("Journal article", 
                             "Journal article; Conference paper",
                             "Conference paper; Journal article")) %>%
  filter(language == "English") 

#Bind wos and cab ---------
wos$wos_id <- as.numeric(wos$wos_id)
dat <- bind_rows(wos, cab)
#How many papers do we have?
nrow(dat)

#Now get proquest. ----------
pro1 <- read.csv("data/proquest1.csv")
pro2 <- read_csv("data/proquest2.csv")
pro1 <- pro1 %>% select(Title, Abstract, StoreId, ArticleType, Authors, digitalObjectIdentifier, 
                        documentType, language, pubdate, pubtitle, year, DocumentURL, subjects)
pro2 <- pro2 %>% select(Title, Abstract, StoreId, ArticleType, Authors, digitalObjectIdentifier, 
                        documentType, language, pubdate, pubtitle, year, DocumentURL, subjects)
pro <- bind_rows(pro1, pro2)
pro <- pro %>% 
  filter(language %in% c(" English", "English")) %>%
  filter(ArticleType == "Scholarly Journals") %>%
  filter(!documentType %in% c(" General Information", " Book Review", " Editorial", " Book review",
                              " Book Monograph , Conference", " PERIODICAL", " Journal Article , Conference",
                              "PERIODICAL", "Book review", " Case report", " Miscellaneous"))
pro <- pro %>% select(Authors, Title, pubtitle, language, ArticleType, subjects, Abstract, 
                        year, digitalObjectIdentifier, StoreId)
pro <- pro %>% add_column(id = rep(NA, nrow(pro)), .before = 1)
pro <- pro %>% add_column(disciplines = pro$subjects, .after = "digitalObjectIdentifier")
pro <- pro %>% mutate(include = NA) %>% mutate(source = "proquest")
names(pro) <- names(dat)

#Fix doi.
pro <- pro %>% mutate(doi = strsplit(pro$doi, split = " , "))
pro$doi <- unlist(sapply(pro$doi, function(x){
  if (length(x[startsWith(x, "10.")]) != 0) {
    x[startsWith(x, "10.")]} else{NA}
})) 

dat <- bind_rows(dat, pro)

#How many unique dois do we have?
length(unique(dat$doi))

#How many unique titles? In a perfect world, this would be the same as number of unique dois.
length(unique(dat$title))

#Throw out duplicate titles and DOI.
#This should preferentially keep web of science, because it's first.
dat_distinct <- dat %>% filter(!is.na(doi)) %>%
  distinct(doi, .keep_all = TRUE) %>%
  distinct(title, .keep_all = TRUE)
doi_na <- dat %>% filter(is.na(doi))

dat_new <- bind_rows(dat_distinct, doi_na) 
#Throw out duplicate titles that have NA doi (only 3)
dat_new <- dat_new %>% distinct(title, .keep_all = TRUE) 

#Now, make exclusions. ----------
source("exclusions_fn.R")
dat_keep <- exclusions_fn(dat_new, abstract = FALSE, rivers = TRUE, keywords = FALSE)
excluded <- dat_new[!dat_new$title %in% dat_keep$title,]
#View(excluded)
dat_keep %>% group_by(source) %>% summarise(count = n())
a <- excluded %>% filter(include == "Yes") 
print(nrow(a))

  
#Add ids. ---------
dat_new <- dat_new %>% arrange(id, desc(source))
n <- max(dat_new$id, na.rm = TRUE)
m <- nrow(dat_new[dat_new$source != "web_of_science",])
new_ids <- c((n+1):(n+m))
dat_new$id[dat_new$source != "web_of_science"] <- new_ids

#Assign reviewers.--------------
names <- c("Adrienne", "Becky", "Danny", "Micah", "Courtney", "Paris", "Shana", "Meghan")
new <- dat_new %>% filter(source != "web_of_science")
n <- ceiling(nrow(new)/length(names))
names <- rep(names, each = n)

dat_new <- dat_new %>% mutate(reviewer = NA)
dat_new$reviewer[dat_new$source != "web_of_science"] <- names[1:nrow(new)]

write_csv(dat_new, "data/wos_cab_pro.csv")
write_csv(excluded, "data/excluded.csv")
