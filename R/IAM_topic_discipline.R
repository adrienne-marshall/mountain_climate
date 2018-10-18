# Make bar charts of topic/discipline covered in impacts, adaptation, and mitigation papers:
# Issue: which topics do we really want? Top n topics total? Top n topics by IAM?
library(tidyverse)
library(tidytext)
library(ggthemes)
library(pals)

case <- "discipline" # discipline
separate_impacts <- F

# These are the topics that Micah and Meghan cleaned and Courtney arbitrated. 
dat <- read_csv("../results/tabular/all_dat_cleaned.csv")

# Split into multiple impact types. 
if(separate_impacts == T){
  dat <- dat %>% 
    mutate(inclusion_IAM = case_when(
      inclusion_IAM == "impacts" & projected == "yes" & observed == "yes" ~ "Projected impacts, Observed Impacts",
      inclusion_IAM == "impacts" & projected == "no" & observed == "no" ~ "Implications",
      inclusion_IAM == "impacts" & projected == "yes" & observed == "no" ~ "Projected impacts",
      inclusion_IAM == "impacts" & projected == "no" & observed == "yes" ~ "Observed impacts",
      inclusion_IAM == "mitigation" ~ "Mitigation",
      inclusion_IAM == "adaptation" ~ "Adaptation")) %>% 
    unnest_tokens(inclusion_IAM, inclusion_IAM, token = stringr::str_split, pattern = ", ") 
}


# Set max number of categories. 
n1 <- 50

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
  group_by(col1, inclusion_IAM) %>% 
  count() %>% 
  ungroup() %>% 
  top_n(n1, wt = n) 

# Summarize data.-------------
summary_dat <- dat %>% 
  mutate(col1 = str_trim(col1)) %>% 
  mutate(col1 = ifelse(col1 %in% top_n1$col1, col1, "other")) %>% 
  group_by(inclusion_IAM, col1) %>% 
  count() %>% 
  ungroup()

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
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 80, hjust = 1)) 
p

pdf(paste0("../results/figures/IAM_", case, ".pdf"),
     width = 7, height = 6)
p
dev.off()

# Make a radar chart ---------------
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

plot_dat <- summary_dat %>% 
  mutate(col1 = ifelse(str_detect(col1, "environmental chem"), "environmental chemistry", col1)) %>% 
  mutate(col1 = str_replace(col1, "/|and| or", "/\n"))

ggplot(plot_dat, 
       aes(x = col1, y = n, color = inclusion_IAM, group = inclusion_IAM)) + 
  geom_polygon(fill = NA, size = 1) + 
  coord_radar() + 
  scale_y_continuous(trans = "log", breaks = c(0, 1, 10, 100, 200)) + 
  scale_color_manual(values = pals::brewer.paired(12)[c(2, 3, 5)]) +   
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.8)) + 
  labs(y = "Number of papers in category")

