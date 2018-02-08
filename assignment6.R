#Assign 10% reviewers for papers in proquest and cabdirect. 
library(tidyverse)

dat1 <- read_csv("data/wos_cab_pro_may8.csv")

dat <- dat1 %>% filter(source != "web_of_science")
names <- unique(dat$reviewer)

dat2 <- dat %>% filter(`Include?` %in% c("Yes", "No", "Maybe", NA))

#Assuming we want to do 10% of the papers that were reasonable (not international)...
n <- nrow(dat2)/(10*length(unique(dat$reviewer)))

#Each person needs to review n papers (18) - or each read two (three?) from one other person.
#dat <- dat %>% mutate(new_reviewer = NA)

#Make assignments.
assignments <- list()
for(i in 1:length(names)){
  assignments[[i]] <- dat2 %>% filter(reviewer == names[i])
  assignments[[i]] <- assignments[[i]][1:ceiling(n),]
  names_temp <- names[names != names[i]]

  assignments[[i]]$new_reviewer <- sample(names_temp, nrow(assignments[[i]]), replace = TRUE)

}

new <- as.data.frame(bind_rows(assignments))

new <- new %>% select(id, new_reviewer)

new <- full_join(dat1, new, by = "id") %>% 
  arrange(id)

write_csv(new, "data/wos_cab_pro_may8_new.csv")
