#Add a second reader to csv. 
library(tidyverse)

dat <- read.csv("articles_jan26.csv")

dat <- dat %>% arrange(X.)

names <- unique(dat$assignment)

#Get rid of an empty element
names <- names[2:9]

for(i in 1:length(names)){
  temp <- dat %>% filter(assignment == names[i])
  names_temp <- as.character(names)
  #Remove the current person. 
  names_temp <- names_temp[names_temp != names[i]]
  
  #Randomly assign.
  temp <- temp[1:21,]
    for (j in 1:nrow(temp)){
  temp$assignment2[j] <- sample(names_temp, 1, replace = TRUE)
    }
  
  temp <- temp %>% select(X., assignment2)
  
  dat <- full_join(dat, temp, by = "X.")
  
}

for (i in 1:nrow(dat)){
  x <- dat[i, 20:27]
  x <- x[!is.na(x)]
  if (length(x) == 0){dat$assignment2[i] <- NA}
  if (length(x) != 0){dat$assignment2[i] <- x}
}

#check that they're evenly distributed. 
ggplot(dat) + geom_histogram(aes(assignment2), stat = "count")

summary <- dat %>% group_by(assignment2) %>% 
  summarise(count = n())
summary


# #Select important rows and write a csv. 
 dat <- dat %>% select(X., wos_id, assignment, assignment2)
# 
 write.csv(dat, "assignment2.csv")
