# Make a bar graph with each discipline - how much are impacts papers observed vs future? 

library(tidyverse)
library(tidytext)
library(pals)
library(ggthemes)

dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

impacts_obs <- dat %>% 
  filter(inclusion_IAM == "impacts") %>% 
  mutate(implications = ifelse(projected == "no" & observed == "no", "yes", "no")) %>% 
  dplyr::select(paper_id, projected, observed, implications, discipline) %>% 
  gather(impact_type, value, -paper_id, -discipline) %>% 
  arrange(paper_id) %>% 
  filter(value == "yes") %>% 
  unnest_tokens(discipline, discipline, token = str_split, pattern = ", ") %>% 
  group_by(discipline, impact_type) %>% 
  count() 

all_poss <- expand.grid(discipline = unique(impacts_obs$discipline),
                        impact_type = unique(impacts_obs$impact_type))

impacts_obs <- impacts_obs %>% right_join(all_poss) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

order <- impacts_obs %>% 
  group_by(discipline) %>% 
  summarise(count = sum(n)) %>% 
  arrange(desc(count)) %>% 
  mutate(index = 1:nrow(.)) %>% 
  dplyr::select(discipline, index)

impacts_obs <- impacts_obs %>% 
  left_join(order, by = "discipline")


p <- ggplot(impacts_obs, aes(x = fct_reorder(discipline, index), y = n, fill = impact_type)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = pals::tol(3)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_few() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank()) +
  labs(y = "Number of papers",
       fill = "Type of \nimpact") 

pdf("../results/figures/impact_type_discipline.pdf",
    width = 9, height = 6)
p
dev.off()
