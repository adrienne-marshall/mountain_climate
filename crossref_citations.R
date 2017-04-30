#Download paper metadata from crossref. 
#install.packages("rcrossref")
require("rcrossref"); library(tidyverse); library(stringr)
library(countrycode); library(mapdata)
source("exclusions_fn.R")

#Only get citations if we don't already have them...

#if(!file.exists("data/crossref_all.csv")){

#Do the search - only get 100 for now. 

papers <- cr_works(query =  "+'climate change' +'mountain' +'columbia river' +'alpine' +'subalpine' 
+'mountainous' +'headwater'  +'washington' +'oregon' +'idaho' +'wyoming' +'montana' +'british columbia' +'kootenay' +'snake' +'salmon' +'clearwater' +'spokane' +'kootenai' +'Pacific Northwest' +'Western United States' +'Columbia River Basin'",
                   filter = c(type = "journal-article"),
                   limit=1000)

papers1 <- cr_works(query = "+'climate change' +'mountain' +'columbia river' +'alpine' +'subalpine' 
+'mountainous' +'headwater'+'washington' +'oregon' +'idaho' +'wyoming' +'montana' +'british columbia' +'kootenay' +'snake' +'salmon' +'clearwater' +'spokane' +'kootenai' +'Pacific Northwest' + 'willamette river'+ 'clark fork river'+ 'john day river'+ 'sandy river'+ 'lewis river'+ 'methow'+ 'white salmon'+'Western United States' +'Columbia River Basin'",  
                    filter = c(type = "journal-article"),
                   limit=1000)

papers2 <- cr_works(query = "+'climate change' +'mountain' +'columbia river' +'alpine' +'subalpine'+'mountainous'+'snow dominant'+'headwater'+'washington' +'oregon' +'idaho' +'wyoming' +'montana' +'british columbia' +'kootenay' +'snake' +'salmon' +'clearwater' +'spokane' +'kootenai' +'Pacific Northwest' + 'willamette'+ 'clark fork'+ 'john day'+ 'sandy'+ 'lewis'+ 'methow'+ 'white salmon'+'Western United States' +'Columbia River Basin'",      
                    filter = c(type = "journal-article"),
                    limit=1000)

papers3 <- cr_works(query = "+'climate change' +'mountain' +'columbia river' +'alpine' +'subalpine' +'mountainous'+'snowdominant'+'headwater'+'washington' +'oregon' +'idaho' +'wyoming' +'montana' +'british columbia' +'kootenay' +'snake' +'salmon' +'clearwater' +'spokane' +'kootenai' +'Pacific Northwest' +'willamette' +'clark fork'+ 'john day'+ 'sandy'+ 'lewis'+ 'methow'+ 'white salmon'+'Western United States' +'Columbia River Basin'+ 'cascade'+'blue'+'selkirk'+'purcell'+'wallowa'+'teton'+'rocky'+'bitterroot'+ 'rockies'",
                    filter = c(type = "journal-article"),
                    limit=1000)

papers4 <- cr_works(query = "+'climate change' +'mountain' +'columbia river' +'alpine' +'subalpine' 
+'mountainous' +'headwater' +'indigenous' + 'knowledge' + 'local' + 'community' + 'Pacific Northwest' +'Western United States' +'Columbia River Basin'",
                    filter = c(type = "journal-article"),
                    limit=1000)

dat <- list(papers1, papers2, papers3, papers4)

dat <- bind_rows(papers1[[2]], papers2[[2]], papers3[[2]], papers4[[2]])


#Funnel down.
data <- dat %>% dplyr::select(alternative.id, container.title, created,
                                   DOI, funder, link, publisher, reference.count,
                                   score, source, subject, title, subtitle, type, URL, author,
                                   issue, volume) 

#Get unique cases only: 4000 unique cases (?!). 
data <- unique(data) %>% arrange(score)
nrow(data)

#Write R object to file with this data, to avoid searching again.
#First need to unlist authors. 
for(i in 1:nrow(data)){
  if(!is.null(data$author[[i]])){
    x <- data$author[[i]]
    if(names(x) == c("given", "family")){
    x <- x %>% mutate(name = paste0(given, " ", family)) %>% select(name)
    }
    if(names(x) == "family"){
      names(x) <- "name"
    }
y <- unlist(x$name)
    data$author[i] <- paste(y, collapse = "; ")
  } else(data$author[i] <- NaN)
}

data$author <- unlist(data$author)
data$author[data$author == ""] <- NaN

#Unlist funder info; get rid of link.
data <- data %>% select(-link)
test <- paste(data$funder, collapse = "; ")
test <- strsplit(test, split = "; ")
test <- test[[1]]
test <- test[1:nrow(data)]
data$funder <- test

write_csv(data, "data/crossref_all.csv")

#}

#Now work more with the data. --------
data <- read_csv("data/crossref_all.csv")

data <- data %>% distinct(title, DOI, .keep_all = TRUE)
nrow(data)

#This gets us down to 1847 rows. 
#Keep only reference counts > 5.
data <- data %>% filter(reference.count > 5)

#Get rid of alternative id; rearrange columns.
data <- data %>% select(-alternative.id) 
data <- data %>% select(title, author, score, everything()) %>% arrange(desc(score))
View(data)

#Now only 866 - very manageable! Use some appropriate exclusions.
data_keep <- exclusions_fn(data, title = TRUE, keywords = FALSE)

#Look at excluded papers. 
excluded <- data[!data$title %in% data_keep$title,]
View(excluded)
#write a csv of excluded papers.
write_csv(excluded, "data/crossref_exclusions.csv")

#Load web of science, proquest, and cabdirect so we can only keep NEW crossref papers.
old <- read_csv("data/wos_cab_pro.csv")
old <- old %>% select(title, doi)
names(old) <- c("title", "DOI")

#get rid of papers crossref found that we already have.
data_new <- data_keep %>% filter(! title %in% old$title) %>%
  filter(! DOI %in% old$DOI)
nrow(data_new)

#assign reviewers.
names <- c("Adrienne", "Becky", "Danny", "Micah", "Courtney", "Paris", "Shana", "Meghan")
n <- ceiling(nrow(data_new)/length(names))
names <- rep(names, each = n)
names <- names[1:nrow(data_new)]

data_new <- data_new %>% mutate(reviewer = names)

#Write a csv.
write_csv(data_new, "data/crossref_papers.csv")



