library(dplyr)

rivers <- read.csv("rivers.csv")
rivers <- rivers %>% select(name)

#Get rid of question marks. 
rivers <- rivers %>% filter(!grepl('\\?', name))

#Separate those in parentheses. 
#test <- unlist(strsplit(rivers, split = '\\('))

#Add the word "river" to each.
rivers <- sapply(rivers, function(x){
  paste0(x, " River")
})

#Take out the rivers we want to keep.
rivers <- rivers[rivers != "Columbia River"]
rivers <- rivers[rivers != "Canadian River"]
rivers <- rivers[rivers != "Clark Fork River"]
rivers <- rivers[rivers != "Flathead River"]
rivers <- rivers[rivers != "John Day River"]
rivers <- rivers[rivers != "Pend Oreille River"]
rivers <- rivers[rivers != "Red River"]
rivers <- rivers[rivers != "Salmon River"]
rivers <- rivers[rivers != "Snake River"]
rivers <- rivers[rivers != "Willamette River"]


return(rivers)



