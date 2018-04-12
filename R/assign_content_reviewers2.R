# Assign content reviewers for a second round. This time, go ahead and assign everything. 

library(tidyverse)

dat <- read_csv("../data/assignments/content_review_round2.csv")

## Separate the part that's not assigned yet.
assigned <- dat %>% filter(reviewer1 != "Z")
unassigned <- dat %>% filter(reviewer1 == "Z")

names <- unique(dat$reviewer1)
names <- names[names != "Meghan"]
names <- c("Meghan", names)
names <- names[names != "Z"]

# How many papers does each person need to code? 
# 92 papers. 
nreps <- floor(nrow(unassigned)/length(names))

# Assign reviewer 1 (account for "claimed territory")
reviewer1 <- rep(names, each = nreps)
nleft <- nrow(unassigned) - length(reviewer1)

unassigned <- unassigned %>% arrange(`Mark your territory`)

unassigned$reviewer1 <- c(reviewer1, rep("Adrienne", nleft))

# Assign reviewer 2

# Assign reviewer 2
dfs <- vector("list", length(names))
last <- "Paris"
n <- length(names)
for(i in 1:length(names)){
  temp <- unassigned %>% filter(reviewer1 == names[i])
  
  if(last != names[6]){
    r <- c(names[(which(names == last) + 1):n], 
           names[1:which(names == last)])
  } else {r <- names}
  r <- r[r != names[i]]
  
  
  temp$reviewer2 <- rep(r, times = ceiling(nreps/length(r)))[1:nrow(temp)]
  dfs[[i]] <- temp
  last <- tail(temp$reviewer2, 1)
}

df <- bind_rows(dfs)

dat_final <- bind_rows(assigned, df)

write_csv(dat_final, "../data/assignments/content_review_all.csv")
