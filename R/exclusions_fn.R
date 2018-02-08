#A function to exclude articles. 
#dat must be a data frame with a column called title, and one called abstract.
#if no abstract column, just set abstract to FALSE.

exclusions_fn <- function(data, 
                       title = TRUE, 
                       abstract = FALSE,
                       keywords = TRUE,
                       countries = TRUE,
                       rivers = TRUE, 
                       states = TRUE, 
                       continents = TRUE){
  
  library(tidyverse); library(stringr)
  library(countrycode)
  
  exclusions <- c()
  
  #Define exclusions by location.
  if(countries == TRUE){
  countries <- countrycode_data$country.name.en
  countries <- countries[countries != "United States of America"]
  countries <- countries[countries != "Canada"]
  #Add a few countries that are weirdly named in the default list. 
  countries <- c(countries, "Iran", "Vietnam", "UK", "Bolivia", "Micronesia", "United Kingdom")
  exclusions <- c(exclusions, countries)
  }
  
  if(states == TRUE){
  states <- state.name
  states_in <- c("Idaho", "Washington", "Oregon", "Nevada", "Utah", "Wyoming", "Montana")
  ans <- states %in% states_in
  states_ex <- states[!ans] 
  exclusions <- c(exclusions, states_ex)
  }
  
  if(rivers == TRUE){
   source("river_names.R")
   exclusions <- c(exclusions, rivers)
  }
   
  if(continents == TRUE){
  continents <- c("Asia", "Africa", "Europe", "South America", "Australia", "Antarctica",
                  "African", "Asian", "European", "South American", "Australian")
  exlusions <- c(exclusions, continents)
  }
  
  test <- data
  for(i in 1:length(exclusions)){
    if(title == TRUE){test <- test %>% filter(!str_detect(title, exclusions[i]))}
    if(abstract == TRUE){test <- test %>% filter(!str_detect(abstract, exclusions[i]))}
    if(keywords == TRUE){test <- test %>% filter(!str_detect(keywords, exclusions[i]))}
  }
  
  return(test)
  
  
}