#Read and clean data from web of science. 

dat1 <- read.table("savedrecs1.txt", header = TRUE, sep = "\t", 
                  fileEncoding = "latin1", fill = TRUE, 
                  stringsAsFactors=FALSE, quote = "",
                  row.names=NULL)
dat2 <- read.table("savedrecs2.txt", header = TRUE, sep = "\t", 
                   fileEncoding = "latin1", fill = TRUE, 
                   stringsAsFactors=FALSE, quote = "",
                   row.names=NULL)
dat3 <- read.table("savedrecs3.txt", header = TRUE, sep = "\t", 
                   fileEncoding = "latin1", fill = TRUE, 
                   stringsAsFactors=FALSE, quote = "",
                   row.names=NULL)
dat4 <- read.table("savedrecs4.txt", header = TRUE, sep = "\t", 
                   fileEncoding = "latin1", fill = TRUE, 
                   stringsAsFactors=FALSE, quote = "",
                   row.names=NULL)
dat <- rbind(dat1,dat2, dat3, dat4)

#Clean based on a look at what's there. 
dat <- data.frame(authors = as.character(dat[,6]),
                   title = dat[,9],
                   journal = dat[,10],
                   language = dat[,13], #maybe we get rid of any that aren't english?
                   type = dat[,14], #maybe we only keep articles??
                   keywords = dat[,20],
                   keywods2 = dat[,21], #not clear what the difference is between these two, but they are different.
                   abstract = dat[,22],
                   year = dat[,45],
                   doi = dat[,55],
                   disciplines = dat[,59],
                   wos_id = dat[,61])
names <- c("Paris", "Micah", "Adrienne", "Becky", 
           "Danny", "Courtney", "Meghan", "Shana")
n <- round(length(dat$authors)/length(names), digits=0)
assignment <- rep(names, each=n)
assignment <- assignment[1:length(dat$authors)]

library(dplyr)
dat <- mutate(dat, assignment=assignment)

dat <- mutate(dat, check = rep(NA, length(dat$authors)))
names <- c(names, "Paris")
for (i in 1:(length(names)-1)){
  x <- filter(dat, assignment == names[i])
  x <- x[1:10,]
  dat$check[dat$doi==x$doi] <- names[i+1]
}

write.csv(dat, "wos_recs.csv", col.names=TRUE)

