#This script reads data from a web of science search, cleans it, and preps it for SNA.
#Author: Adrienne Marshall
#Date: November 2016

#Read and clean data from web of science. 
rm(list=ls())
#Read the data. Change name of the file if you saved it differently.
dat <- read.table("wos_recs_Jan9.tsv", 
                  header = TRUE, 
                  sep = "\t", 
                  #fileEncoding = "latin1", 
                  fill = TRUE, 
                  stringsAsFactors=FALSE, 
                  quote = "",
                  row.names = NULL)
#Clean the data; name it "data."
data <- data.frame(author = as.character(dat[,2]),
                   title = dat[,3],
                   journal = dat[,4],
                   language = dat[,5], #maybe we get rid of any that aren't english?
                   type = dat[,6], #maybe we only keep articles??
                   keywords = dat[,7],
                   keywords2 = dat[,8], #not clear what the difference is between these two, but they are different.
                   abstract = dat[,9],
                   institutions = dat[,23],
                   refs = dat[,30],
                   publisher = dat[,36],
                   id = dat[,39],
                   year = dat[,10],
                   doi = dat[,55],
                   disciplines = dat[,59],
                   wos_id = dat[,61])

for (i in 1:17){
  data[,i] <- as.character(data[,i])
}
data$year <- as.numeric(data$year)
#Next steps... make a matrix for SNA?
#Make a list of abstracts and clean them? not sure it needs to be separate though.

#Make a matrix for SNA.-------------------------------------------
#First, for each paper, need a list of authors. 
x <- data.frame(title = character(),
                author = character())
for (i in 1:length(data$title)){
  authors <- unlist(strsplit(data$author[i], split = ";"))
  temp <- data.frame(title = rep((data$title[i]), length(authors)),
                     author = authors)
  x <- rbind(x,temp)
}

#Separate author first and last.
library(dplyr)
x$author <- as.character(x$author)
for (i in 1:length(x$author)){
  x$last[i] <- strsplit(x$author, split = ",")[[i]][1]
  x$first[i] <- strsplit(x$author, split = ",")[[i]][2]
}

#Identify possible matching authors - need to look at this output manually.
names <- character()
names2 <- character()
x <- arrange(x,last, first,title)
for (i in 2:length(x$author)){
  if (x$last[i]==x$last[i-1] && x$first[i]!=x$first[i-1]){ #identifying different first and last.
    names[i] <- x$author[i]
    names2[i] <- x$author[i-1]
  }
}
namesdf <- data.frame(names = names, names2=names2)
namesdf <- filter(namesdf, !is.na(names2))
namesdf

#Now need to replace doubles by hand. 
#Write a data frame that lists all the replacements - just did a few here to test.
#Would be worth thinking about whether there's a better way to do this...
#Could grab the first letter of the first name, and assume if they match, it's the same person??
#THat would be: substring(x$first[i],1,2)
#They all have spaces in front of them. 
right <- c(" Aitken, Sally N.", " Alfaro, Rene", " Allen, Sandra")
wrong <- c(" Aitken, SN", " Alfaro, R.I.", " Allen, S")

#For authors who are listed in multiple ways, change them to one version of the name.
for (i in 1:length(right)){
  x$author[x$author == wrong[i]] <- right[i]
}


#------------------------------------------------------------------------------
#For each unique author, make a list of all the authors they have cited. 
#Start by adding the references list to the "x" data frame.
for (i in 1:length(x$author)){
  x$refs[i] <- data$refs[data$title==x$title[i]]
}
authors <- unique(x$author)

for (i in 1:length(x$author)){
  #For each reference, find matches with author names.
  a <- strsplit(x$refs[i], slit=" ")
}


#Make a matrix: For each author, put a 1 if they have cited each other author. 







