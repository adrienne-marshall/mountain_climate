## Make some plots and stats summarising our data. 
library(tidyverse)
library(ggthemes)
library(stringr)
library(tidytext)

dat <- read_csv("data/final_corpus.csv")

dat <- dat %>%
  mutate_all(tolower)

## Summary stats: number of papers. Distinct number of journals? ----
npapers <- nrow(dat)
njournals <- length(unique(dat$journal))

#authors per paper
authors_per_paper <- dat %>%
  select(id, authors, year) %>% 
  unnest_tokens(output = authors, input = authors, token = str_split, pattern = ";") %>%
  group_by(id, year) %>%
  count() %>%
  ungroup() %>%
  mutate(year = as.numeric(year))
mean(authors_per_paper$n)
sd(authors_per_paper$n)
plot_dat <- authors_per_paper %>%
  group_by(year) %>%
  summarise(n = mean(n))

ggplot(plot_dat, aes(x = year, y = n)) + geom_point() + geom_smooth()

author_df <- read_csv("data/authors.csv")

#number of authors:
length(unique(author_df$author))

#papers per author:
p_p_a <- author_df %>% group_by(author) %>% count()
mean(p_p_a$n)


#top 10 journals:
dat %>% group_by(journal) %>%
  count() %>% 
  ungroup() %>%
  arrange(desc(n))

## Figure: number of papers per year----------
annual <- dat %>% group_by(year) %>% 
  filter(year < 2017) %>%
  count() %>% 
  ungroup() %>%
  filter(!is.na(year)) %>%
  mutate(year = as.numeric(year))

p <- ggplot(annual, aes(x = year, y = n)) + 
  geom_point() +
  geom_line() +
  theme_few() + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  labs(x = "Year", y = "Number of papers")

tiff("figures/publications_per_year.tiff", res = 300, units = 'in', width = 9, height = 6)
p
dev.off()

## Figure: Trajectories of most popular keywords--------
keyword_use <- dat %>% 
  unnest_tokens(output = keywords_long, input = keywords, token = stringr::str_split,
                pattern = ";") %>%
  select(year, keywords_long) %>%
  na.omit() %>%
  mutate(keywords_long = str_trim(keywords_long))
#Get top keywords.
top_kw <- keyword_use %>%
  group_by(keywords_long) %>%
  count() %>% 
  arrange(desc(n))
top_kw <- top_kw[1:15,]

keywords <- keyword_use %>%
  filter(keywords_long %in% c(top_kw$keywords_long)) %>%
  group_by(year, keywords_long) %>%
  count() %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2017)


years_vect <- (min(keywords$year):max(keywords$year))

years <- data.frame(year = rep(years_vect, times = nrow(top_kw)),
                    keywords_long = rep(top_kw$keywords_long, each = length(years_vect)))
plot_dat <- left_join(years, keywords, by = c("year", "keywords_long")) 
plot_dat$n[is.na(plot_dat$n)] <- 0

levels(plot_dat$keywords_long) <- top_kw$keywords_long

p <- ggplot(plot_dat, aes(x = year,y = n, group = keywords_long, fill = keywords_long)) +
  geom_area(aes(fill = keywords_long), 
            position = 'fill', color = "grey80", size = 0.1) +
  theme_few() +
  theme(panel.border = element_blank()) +
  labs(fill = "Keywords",
       y = "Fraction",
       x = "Year")

tiff("figures/keywords_area.tiff", width = 8, height = 6, res = 300, units = 'in')
p
dev.off()

## Plot a sideways bar chart of most frequent keywords.------------
# Group some keywords.
climate_group <- c("climate change", "climate", "global warming")
mountain_group <- c("mountains", "mountain", "mountain areas")
keyword_use$keywords_long[keyword_use$keywords_long %in% climate_group] <- "climate change"
keyword_use$keywords_long[keyword_use$keywords_long %in% mountain_group] <- "mountain"


top_kw <- keyword_use %>%
  group_by(keywords_long) %>%
  count() %>% 
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(keywords_long = reorder(keywords_long, n))
top_kw <- top_kw[1:30,]

#make a plot. 
keywords_simple <- ggplot(top_kw, aes(x = keywords_long, y = n)) +
  geom_col(fill = "salmon") + 
  coord_flip() +
  theme_minimal() +
  #scale_y_continuous(trans = 'log') +
  labs(x = NULL,
       title = "Top 30 keywords",
       y = "Number of occurrences") +
  theme(text = element_text(size = 14))

tiff("figures/keyword_counts.tiff", width = 8, height = 9, res = 300, units = 'in')
keywords_simple
dev.off()


## Fill in plot by year. ---------
keywords <- keyword_use %>%
  filter(keywords_long %in% top_kw$keywords_long) %>%
  left_join(top_kw, by = "keywords_long") %>%
  mutate(keywords_long = reorder(keywords_long, n))

# Group by year
keywords <- keywords %>%
  mutate(year_range = case_when(year %in% c(1985:1989) ~ "1985-1989",
                          year %in% c(1990:1994) ~ "1990-1994",
                          year %in% c(1995:1999) ~ "1995-1999",
                          year %in% c(2000:2004) ~ "2000-2004",
                          year %in% c(2005:2009) ~ "2005-2009",
                          year %in% c(2010:2014) ~ "2010-2014",
                          year %in% c(2015:2017) ~ "2015-2017"))
keywords <- keywords %>%  mutate(keywords_long = reorder(keywords_long, n))



colored_keywords <- 
  ggplot(keywords, aes(x = keywords_long, fill = year_range)) +
  geom_bar() +
  coord_flip() +
  #scale_y_sqrt() +
  scale_fill_viridis(option = "B", discrete = TRUE, direction = -1) +
  theme_minimal() +
  labs(x = NULL,
       title = "Top keywords",
       fill = "Year",
       y = "Number of occurrences") +
  theme(legend.position = c(0.86, 0.5),
        legend.box.background = element_rect(fill = "white", color = "white"))
 
tiff("figures/colored_keywords.tiff", width = 9, height = 6, res = 300, units = 'in')
colored_keywords
dev.off()

## make a version without climate keywords. 
keywords <- keywords %>% filter(keywords_long != "climate change")

keywords_noclimate <- ggplot(keywords, aes(x = keywords_long, fill = year_range)) +
  geom_bar() +
  coord_flip() +
  scale_fill_viridis(option = "B", discrete = TRUE, direction = -1) +
  theme_minimal() +
  labs(x = NULL,
       title = "Top keywords",
       fill = "Year",
       y = "Number of occurrences",
       caption = "*Chart excludes \"climate\", \"climate change\", and \"global warming\", which occurred a total of 596 times. 
The \"mountain\" term includes \"mountains\", \"mountain\", and \"mountain areas\".") +
  theme(legend.position = c(0.86, 0.5),
        legend.box.background = element_rect(fill = "white", color = "white"),
        plot.caption = element_text(hjust = 0))

tiff("figures/colored_keywords_noclimate.tiff", width = 9, height = 6, res = 300, units = 'in')
keywords_noclimate
dev.off()

