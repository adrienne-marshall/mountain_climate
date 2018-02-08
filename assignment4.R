readers <- c("Adrienne", "Becky", "Courtney", "Danny", 
           "Meghan", "Micah", "Paris", "Shana")
readers <- rep(readers, 203)
readers <- sample(readers)

round4 <- write.csv(readers, "round4.csv", row.names=FALSE)

