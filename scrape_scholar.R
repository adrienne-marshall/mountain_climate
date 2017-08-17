library(rvest)
library(stringr)
library(glue)

source("http://biostat.jhsph.edu/~jleek/code/googleCite.r") ##work more with these functions to get scholar IDs.

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
for(i in 1:10){#nrow(authors)){
  try({
    data <- search_cite2(authors$full[i])
    authors$text[i] <- data
  })
    print(i)
} #I think this got rate-limited... try again later?

write_csv(authors, "data/author_data606.csv")

#Now extract data:
#Not sure yet how to do this for multiple search results
for(i in 1:nrow(authors)){
  if(nchar(authors$text[i])>1)
    y <- authors$text[2]
    name <- str_extract_all(y, "span class='gs_hlt'>.*</span>")
  name <- unlist(name)
  name <- gsub("span class='gs_hlt'>", "", name)
  name <- gsub("</span>", "", name)
}



#writing myself from here - 
if (length(x) > 1){ ### if they have someone for a hit
  name <- str_extract_all(x, "span class='gs_hlt'>.*</span>")
  name <- unlist(name)
  name <- gsub("span class='gs_hlt'>", "", name)
  name <- gsub("</span>", "", name)
  

    
out2 <- searchCite("John Abatzoglou") #this doesn't work, but can borrow from this function.


affiliation <- page %>%
  html_nodes(".gsc_prf_il") %>%
  html_text()


  

page <- read_html("https://scholar.google.com/citations?user=S1J4kAoAAAAJ&hl=en")





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
