# Make bar charts of topic/discipline covered in impacts, adaptation, and mitigation papers:
library(tidyverse)
library(tidytext)
library(ggthemes)
library(pals)

case <- "discipline" # discipline

dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Set max number of categories. 
n1 <- 3

# Rename interest column to "col1".----------
if(case == "topic"){
  dat <- dat %>% rename(col1 = topic)
}
if(case == "discipline"){
  dat <- dat %>% rename(col1 = discipline)
}

dat <- dat %>% 
  unnest_tokens(col1, col1, token = str_split, pattern = ", ") %>% 
  mutate(col1 = str_trim(col1))

# Get top n. ------
top_n1 <- dat %>% 
  group_by(inclusion_IAM, col1) %>% 
  count() %>% 
  top_n(n1, wt = n) 

# Summarize data.-------------
summary_dat <- dat %>% 
  mutate(col1 = str_trim(col1)) %>% 
  group_by(inclusion_IAM, col1) %>% 
  count() 

summary_dat <- inner_join(summary_dat, top_n1)

# Summarise by proportions. 
summary_dat <- summary_dat %>% 
  left_join(dat %>% 
              filter(col1 %in% top_n1$col1) %>% 
              group_by(inclusion_IAM) %>% 
              count() %>% 
              rename(n_papers = n),
            by = "inclusion_IAM") %>% 
  mutate(percent = 100*n/n_papers)

# Make a plot. ----------
p <- ggplot(summary_dat, 
       aes(x = inclusion_IAM, y = percent, fill = col1)) + 
  geom_col(position = "stack", alpha = 1) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = pals::tol(20)) + 
  labs(fill = case,
       y = "% of papers") + 
  theme_few() + 
  theme(axis.title.x = element_blank(),
        panel.border = element_blank()) 

pdf(paste0("../results/figures/IAM_", case, ".pdf"),
    width = 7, height = 6)
p
dev.off()

