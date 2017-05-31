#Assign 10% reviewers for crossref data. 
library(tidyverse)

dat <- read_csv("data/crossref_0531.csv")
names(dat)[1] <- "title"
names(dat)[names(dat) == "Include?"] <- "Include"

#Only review papers that weren't international. 
dat <- dat %>% filter(!Include %in% c("International", "Paleo"))

reviewers <- c("Adrienne", "Meghan", "Shana", "Micah", "Danny", "Courtney")

n <- nrow(dat)/(10*length(reviewers))

#Make assignments.
assignments <- list()
for(i in 1:length(reviewers)){
  assignments[[i]] <- dat %>% filter(reviewer == reviewers[i])
  assignments[[i]] <- assignments[[i]][1:ceiling(n),]
  names_temp <- reviewers[reviewers != reviewers[i]]
  
  assignments[[i]]$new_reviewer <- sample(names_temp, nrow(assignments[[i]]), replace = TRUE)

}

new <- as.data.frame(bind_rows(assignments))

new <- new %>% select(title, new_reviewer)

new <- full_join(dat, new, by = "title") %>% 
  arrange(title)

new %>% group_by(new_reviewer) %>% 
  summarise(count = n())

write_csv(new, "data/crossref_0531_new.csv")

