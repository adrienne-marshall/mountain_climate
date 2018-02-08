# Assign content reviewers. 
library(tidyverse)

# Round 1:-----------------
dat <- read_csv("data/final_corpus.csv")
dat <- dat[, !grepl("Reviewer", names(dat))]
dat <- dat %>% arrange(`Mark your territory`)
dat <- dat[, !names(dat) %in% c("X18", "X19")]

dat_original <- dat

# Assign reviewers 1 and 2 for 1/3 of papers:
reviewers <- c("Adrienne", "Danny", "Micah", "Meghan", "Courtney", "Shana", "Paris")
num <- ceiling(nrow(dat)/3)  # number of papers total for this round
ppp <- ceiling(num/length(reviewers)) # papers per person

# Take the first 40 that MF claimed, then num-ppp more.
x <- num - ppp
unclaimed <- dat[is.na(dat$`Mark your territory`),]
dat <- bind_rows(dat[1:ppp,], unclaimed)

# Assign reviewer 1. 
dat <- dat %>% mutate(reviewer1 = NA)
dat$reviewer1[1:ppp] <- "Meghan"
dat$reviewer1[(ppp+1):280] <- rep(reviewers[reviewers != "Meghan"], each = ppp)
dat <- dat %>% filter(!is.na(reviewer1))

# Assign reviewer 2
dat <- dat %>% mutate(reviewer2 = NA)
dfs <- vector("list", length(reviewers))
last <- "Paris"
n <- length(reviewers)
for(i in 1:length(reviewers)){
  temp <- dat %>% filter(reviewer1 == reviewers[i])
  
  if(last != reviewers[7]){
  r <- c(reviewers[(which(reviewers == last) + 1):n], 
             reviewers[1:which(reviewers == last)])
  } else {r <- reviewers}
  r <- r[r != reviewers[i]]
 
  
  temp$reviewer2 <- rep(r, times = ceiling(ppp/length(r)))[1:nrow(temp)]
  dfs[[i]] <- temp
  last <- tail(temp$reviewer2, 1)
}

df <- bind_rows(dfs)

unassigned <- dat_original[!dat_original$final_id %in% df$final_id, ]

dat_final <- bind_rows(df, unassigned)

write_csv(dat_final, "data/content_review_round1.csv")

