#Check kappa statistics on web of science searches and reviews. 

rm(list=ls())
library(tidyverse); library(psych)

wos <- read.csv("data/wos_recs.csv")

wos <- wos %>% 
  dplyr::select(X, title, Round.1..Include., Round.2..Include., Round.3..Include., 
                Round.4..Inlude., Round.5..Include.)

names(wos) <- c("id", "title", "1", "2", "3", "4", "5")
wos[wos == ""] <- NA

rounds <- names(wos)[3:7]
results <- data.frame(first = as.numeric(rep(rounds, length(rounds))), 
                      second = as.numeric(rep(rounds, each = length(rounds))))
results <- mutate(results, kappa = NA)
results <- results %>% filter(first < second) %>% arrange(first, second)
#Take out cases where none will match.
results <- results[-5,]

#Change "International" values to "No."
wos[wos == "International"] <- "No"

for (i in 1:nrow(results)){
  temp <- wos[,c(results$first[i]+2, results$second[i]+2)]
  temp <-  na.omit(temp)
 ans <- cohen.kappa(temp)
 results$kappa[i] <- ans$kappa
}

results$kappa <- round(results$kappa, 3)

write_csv(results, "data/kappa_results.csv")




