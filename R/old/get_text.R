#Working on getting and cleaning text...
#Biggest problem so far: getting stuff behind a paywall.

#This code gets html - hasn't worked for most articles I've tried.
con = url("http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146004")
htmlCode = readLines(con)
close(con)
htmlCode

#Doing it with XML...
library(XML)
url <- "http://iopscience.iop.org/article/10.1088/1748-9326/11/7/074010/meta"
html <- htmlTreeParse(url, useInternalNodes = T)
xpathSApply(html, "//title",xmlValue) #would need to look at xml structures to figure this out.

#Use the httr package...
library(httr)
html2 = GET(url)
content2 = content(html2, as="text")
parsedHtml = htmlParse(content2, asText=TRUE)
xpathSApply(parsedHtml, "//title",xmlValue)

#Accessing websites with passwords:
pg2 = GET("http://httpbin.org/basic-auth/user/passwd",
          authenticate("user", "password"))
pg2

#Using handles
google = handle("http://google.com")
pg1 = GET(handle=google, path = "/")
pg2 = GET(handle = google, path = "search")

#Working with xml...
library(XML)
url <- "http://iopscience.iop.org/article/10.1088/1748-9326/11/7/074010/meta"
html <- htmlTreeParse(url, useInternalNodes = T)
rootNode <- xmlRoot(html)
xmlName(rootNode)
names(rootNode)
rootNode[[2]][[5]]
xmlSApply(rootNode,"//title",xmlValue) #not working


#Reading from text file - exported from Web of Science.
#Specs: tab-delimited mac text file, used all records and citations.
dat <- read.table("savedrecs.csv", header = FALSE, sep = "\t",fileEncoding = "latin1", fill=TRUE,stringsAsFactors=FALSE)

#Or - if we don't want to touch excel - 
dat <- read.table("savedrecs.txt", header = TRUE, sep = "\t", 
                  fileEncoding = "latin1", fill = TRUE, stringsAsFactors=FALSE, quote = "")

