#Add a second reader to csv. 
library(tidyverse)


dat <- read.csv("articles_mar20.csv")

dat <- dat %>%
  select(X, Round.4..Assignment) 
names(dat) <- c("id", "round4")

dat$id <- as.numeric(as.character(dat$id))
dat <- dat %>% arrange(id) %>% filter(id != "NA")

#Get rid of an empty element
names <- c("Adrienne", "Becky", "Courtney", "Danny", "Meghan", "Micah", "Paris", "Shana")
#bag <- rep(names, ceiling(0.1*nrow(dat)/length(names)))


for(i in 1:length(names)){
  temp <- dat %>% filter(round4 == names[i]) %>% 
    mutate(round5 = "NA")
  #Remove the current person. 
  names_temp <- names[names != names[i]]
  #current <- bag[bag == names[i]]
  
  #Randomly assign.
  temp <- temp[1:ceiling(nrow(temp)/10),]
  for (j in 1:nrow(temp)){
    temp$round5[j] <- sample(names_temp, 1, replace = TRUE)
  }
  
  temp <- temp %>% select(id, round5)
  
  dat <- full_join(dat, temp, by = "id")
  
  bag <- c(names_temp, current)
}

dat <- mutate(dat, round5 = NA)

for (i in 1:nrow(dat)){
  x <- dat[i, 4:11]
  x <- x[!is.na(x)]
  if (length(x) == 0){dat$round5[i] <- NA}
  if (length(x) != 0){dat$round5[i] <- x}
}

#check that they're evenly distributed. 
#ggplot(dat) + geom_histogram(aes(round5), stat = "count")

summary <- dat %>% group_by(round5) %>% 
  summarise(count = n())
summary

range(summary$count)
sd(summary$count)


#Run with the best seed. 


# # #Select important rows and write a csv. 
dat <- dat %>% select(id, round4, round5)
# # 
write.csv(dat, "assignment5.csv", row.names = FALSE, col.names = FALSE)
