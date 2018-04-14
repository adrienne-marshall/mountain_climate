#This is designed to be a flexible version of bibliometrics - 
#coerce our data into the format used by bibliometrix, and prepare for plotting with ggraph. 

library(bibliometrix); library(tidyverse)
library(igraph)

#First, use sample data to make a template. ---------
D<- readLines("data/test.bib")
#Convert to a data frame.
M <- convert2df(D, dbsource = "isi", format = "bibtex")
results_test <- biblioAnalysis(M, sep = ";")
#S_test <- summary(object = results_test, k = 10, pause = FALSE)

#Now get our data. -----
dat <- read_csv("data/wos_cab.csv")
dat <- dat %>% filter(include == "Yes") %>%
  select(-source, -reviewer, -id, -language, -doi, -wos_id, -include)
names(dat) <- c("AU", "TI", "SO", "DT", "DE", "AB", "PY", "SC")

dat <- dat %>% mutate_each(funs(toupper))

#Bibliometric analysis -----
#Consider changing sep if needed with more data sources. 
results <- biblioAnalysis(dat, sep = ";")
#S=summary(object = results, k = 10, pause = FALSE)

#Make igraph objects. -------
#This is where we can set what type of analysis we're interested in. 
#Needs countries: we'll set it up with fake countries for now.
#Later, probably need to pull in more information for these references. 
#dat <- dat %>% mutate(AU_CO = "USA")
dat <- dat %>%  mutate(AU = gsub(",", "", dat$AU)) %>%
  mutate(AU = gsub("\\.", "", dat$AU)) %>%
  mutate(AU = gsub("; ", " ;", dat$AU))

netmat <- biblioNetwork(M, 
                           analysis = "collaboration", 
                           network = "authors", 
                           sep = ";")

A <- cocMatrix(M, Field = "AU", sep = ";")
#Grr... not really working. Might need to pull in a clean version of the data.

