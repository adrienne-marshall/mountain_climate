#Add a second reader to csv. 
library(tidyverse)

dat <- read.csv("articles_jan31.csv")

dat <- dat %>%
  select(X., Assignment.Round.1, Round.2..Assignment) 
names(dat) <- c("id", "round1", "round2")

dat$id <- as.numeric(as.character(dat$id))
dat <- dat %>% arrange(id) %>% filter(id != "NA")

names <- unique(dat$round1)

#Get rid of an empty element
names <- c("Adrienne", "Becky", "Courtney", "Danny", "Meghan", "Micah", "Paris", "Shana")

bag <- rep(names, 21)

set.seed(100)
for(i in 1:length(names)){
  temp <- dat %>% filter(round1 == names[i]) %>% filter(is.na(round2))
  #Remove the current person. 
  names_temp <- names[names != names[i]]
  
  #Randomly assign.
  temp <- temp[1:21,]
  for (j in 1:nrow(temp)){
    temp$round3[j] <- sample(names_temp, 1, replace = TRUE)
  }
  
  temp <- temp %>% select(id, round3)
  
  dat <- full_join(dat, temp, by = "id")
  
}

dat <- mutate(dat, round3 = NA)

for (i in 1:nrow(dat)){
  x <- dat[i, 4:11]
  x <- x[!is.na(x)]
  if (length(x) == 0){dat$round3[i] <- NA}
  if (length(x) != 0){dat$round3[i] <- x}
}

#check that they're evenly distributed. 
ggplot(dat) + geom_histogram(aes(round3), stat = "count")

summary <- dat %>% group_by(round3) %>% 
  summarise(count = n())
summary


# # #Select important rows and write a csv. 
 dat <- dat %>% select(id, round1, round3)
# # 
 write.csv(dat, "assignment3.csv", row.names = FALSE, col.names = FALSE)
