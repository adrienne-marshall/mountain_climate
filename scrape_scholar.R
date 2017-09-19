library(rvest)
library(stringr)
library(tidyverse)

#source("http://biostat.jhsph.edu/~jleek/code/googleCite.r") ##work more with these functions to get scholar IDs.

#Get author data frame.----------
authors <- read_csv("data/authors.csv") #id is just article id. 
#Get longest version of each author name:
authors <- authors %>% 
  mutate(length = nchar(authors_long)) %>%
  group_by(author) %>%
  arrange(desc(length)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-length)
#Get rid of anything after first name after comma.
authors <- authors %>%
  mutate(last = str_extract(authors_long, "[^,]*"),) %>%
  mutate(first = str_extract(authors_long, ",.*"),) %>%
  mutate(first = gsub(", ", "", first)) %>%
  mutate(first = ifelse(grepl(" ", first) == TRUE, 
                        str_extract(first, ".* "),
                        first)) %>%
  arrange(authors_long) %>%
  mutate(full = paste0(first, " ", last))

for(i in 1:nrow(authors)){
  z <- unlist(strsplit(authors$full[i], split = " "))
  name <- paste0(z[1], " ", z[length(z)])
  authors$full[i] <- name
}

#Function to read data from internet: -----------
search_cite2 <- function(Author, ...){
  auth.names <- strsplit(Author, " ")[[1]]
  auth.names <- paste(auth.names[1:length(auth.names)], sep="", collapse="+")
  
  search.page <- paste("http://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=", auth.names, sep="")
  thepage <- url(search.page)
  x <- readLines(thepage)
  close(thepage)
  return(x)
}

#For each author, try to get their google scholar page.
authors <- authors %>% mutate(text = NA)
for(i in 1433:nrow(authors)){
  #try({
    data <- search_cite2(authors$full[i])
    authors$text[i] <- data
  #})
    print(i)
} #I think this got rate-limited... try again later?

write_csv(authors, "data/author_data1433_1966.csv")

##Now read saved data and extract info: --------
files <- list.files(path = "data", pattern = "author_data", full.names = TRUE)
file_dat <- lapply(files, read_csv)
dat <- bind_rows(file_dat)


#Get authors where we do have text:
dat <- dat %>%
  filter(!is.na(text)) %>%
  distinct(full, .keep_all = TRUE)

#write text files for viewing text if necessary:
k <- 11
write_tsv(data.frame(dat$text[k]), "test_char.txt")

#Now extract data:-------
#Not sure yet how to do this for multiple search results
for(i in 1:nrow(dat)){
  
  #If there's text....
  if(nchar(dat$text[i])>1){
    y <- dat$text[i]
  #First get name:
    name <- str_extract_all(y, "span class='gs_hlt'>.*</span>")
    name <- unlist(name)
    name <- gsub("span class='gs_hlt'>", "", name)
    name <- gsub("</span>", "", name)
  
  #Get affiliation:
   aff <- str_extract_all(y, "class=\"gsc_1usr_aff\">.*</div><div class=\"gsc_1usr_eml\"")
   aff <- unlist(aff)
   aff <- gsub("class=\"gsc_1usr_aff\">", "", aff)
   aff <- gsub("</div><div class=\"gsc_1usr_eml\"", "", aff)
   
   #Get self-issued labels:
  # "mauthors=label:climatology\">Climatology</a> <a class=\"gsc_co_int\" href=\"/citations?view_op=search_authors&amp;hl=en&amp;oe=ASCII&amp;mauthors=label:meteorology\">Meteorology</a> </div></div></div><div id=\"gs_ftr\""
   disc <- str_extract_all(y, "label:.{2,20}\\\">")
   disc <- unlist(disc)
   disc <- gsub("label:", "", disc)
   disc <- gsub("\\\">", "", disc)
   disc <- glue::collapse(disc, sep = "; ")
  
  } # end of if statement
} #end of loop through authors

## Alternative: read page with rvest. -------------------
authors <- authors %>% mutate(html = NA, 
                              name_from_web = NA,
                              affiliation = NA,
                              discipline = NA)
# currently this is only getting the first author on a page, and first discipline. 
for(i in 1:10){#nrow(authors)){
  try({
  auth.names <- strsplit(authors$full[i], " ")[[1]]
  auth.names <- paste(auth.names[1:length(auth.names)], sep="", collapse="+")
  search.page <- paste("http://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=", auth.names, sep="")
  page <- read_html(search.page)
  
  name <- page %>%
    html_nodes(".gs_hlt") %>% #.gs_lusr_name #.gs_hlt
    html_text()
  if(length(name) > 0){authors$name_from_web[i] <- name}
  
  aff <- page %>%
    html_nodes(".gsc_lusr_aff") %>%
    html_text() 
  if(length(aff) > 0){authors$affiliation[i] <- aff}
  
  disc <- page %>%
    html_nodes(".gsc_co_int") %>%
    html_text()
  if(length(disc) > 0){authors$discipline[i] <- disc}
  
  authors$html[i] <- page
  }) #end of try statement.
  print(i)
}


## name: .gs_hlt
## affiliation: .gs_lusr_aff
## discipline: .gsc_co_int

### dump code: --------------------------
  
affiliation <- page %>%
  html_nodes(".gsc_prf_il") %>%
  html_text()


  







##tutorial... -----------
citations <- page %>% 
  html_nodes ("#gsc_a_b .gsc_a_c") %>%
  html_text()%>%as.numeric()

page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
Coauthors = page%>% html_nodes(css=".gsc_1usr_name a") %>% html_text()
Coauthors = as.data.frame(Coauthors)
names(Coauthors)='Coauthors'

page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
citations = page%>% 
  html_nodes(css = ".gsc_1usr_cby")%>%html_text()
citations 

citations = gsub('Cited by ','', citations)
citations

citations = as.numeric(citations)
citations = as.data.frame(citations)

page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
affilation = page %>% html_nodes(css = ".gsc_1usr_aff")%>%html_text()
affilation = as.data.frame(affilation)
names(affilation)='Affilation'

cauthors=cbind(Coauthors, citations, affilation)
cauthors
