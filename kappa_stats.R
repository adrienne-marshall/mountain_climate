#Check kappa statistics on web of science searches and reviews. 

#the later part of this script replaces "aggregate_sources" 

rm(list=ls())
library(tidyverse); library(psych)

wos <- read.csv("data/backups/wos_recs.csv")

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
wos[wos == "Paleo"] <- "No"

for (i in 1:nrow(results)){
  temp <- wos[,c(results$first[i]+2, results$second[i]+2)]
  temp <-  na.omit(temp)
 ans <- cohen.kappa(temp)
 results$kappa[i] <- ans$kappa
}

results$kappa <- round(results$kappa, 3)

write_csv(results, "data/kappa_results.csv")

##Get kappa stats on wos_cab_pro.--------
#These are only for cabdirect and proquest. 
dat <- read_csv("data/wos_cab_pro_0606.csv")
names(dat)[names(dat) == "Include?"] <- "include1"
names(dat)[names(dat) == "Include?_1"] <- "include2"
names(dat)[names(dat) == "New reviewer: No international"] <- "reviewer2"
cab_pro <- dat %>% filter(source != "web_of_science") %>%
  select(id, title, reviewer2, include1, include2) %>%
  filter(!is.na(include2))


#Change "International" and "Paleo" to "No".
cab_pro$include1[cab_pro$include1 %in% c("International", "Paleo")] <- "No"
cab_pro$include2[cab_pro$include2 %in% c("International", "Paleo")] <- "No"

#Calculate kappa. 
kap_dat <- cab_pro %>% select(include1, include2)
kap_dat <- data.frame(kap_dat)
ans <- cohen.kappa(kap_dat)

ans$kappa
#So we got 0.73 kappa... pretty good!

#keep a data frame with all cabpro:
cab_pro <- dat %>% filter(source != "web_of_science") %>%
  select(id, title, reviewer2, include1, include2)

#Get kappa stats on crossref. -------------
dat <- read_csv("data/crossref_0711.csv")
names(dat)[names(dat) == "Include?"] <- "include1"
names(dat)[names(dat) == "Include?_1"] <- "include2"

cross <- dat %>% select(title, Reviewer, include1, include2)
cross$include1[cross$include1 %in% c("Paleo", "International")] <- "No"
cross$include2[cross$include2 %in% c("Paleo", "International")] <- "No"
ans <- cross %>% select(include1, include2) %>% data.frame() %>% cohen.kappa()

ans$kappa
#Kappa is 0.63 for this one.

#Get all papers together: ---------
wos_df <- wos %>% dplyr::select(title, `4`,`5`) %>% 
  mutate(source = "wos")
names(wos_df) <- c("title", "include1", "include2", "source")
cabpro_df <- cab_pro %>% dplyr::select(title, include1, include2) %>% mutate(source = "cabpro")
cross_df <- cross %>% select(title, include1, include2) %>% mutate(source = "crossref")

dat_all <- bind_rows(wos_df, cabpro_df, cross_df)
dat_all$include1[dat_all$include1 %in% c("International", "Paleo")] <- "No"
dat_all$include2[dat_all$include2 %in% c("International", "Paleo")] <- "No"

ans <- dat_all %>% select(include1, include2) %>%
  data.frame() %>%
  cohen.kappa()

ans$kappa
#Unweighted kappa is now 72.8% (+/- 8%)

keep <- dat_all %>% filter(include1 %in% c("Yes", "Maybe", NA))
keep %>% 
  group_by(source) %>% 
  count()
#with this done, don't really need aggregate_sources. add a litte more info: 

