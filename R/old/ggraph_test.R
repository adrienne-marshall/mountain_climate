#Get bsk.network from bibliometrics.R

#install.packages("ggraph")

library(ggraph); library(igraph)

dat <- bsk.network

#igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 'randomly', 'fr', 'kk', 'drl', 'lgl')

ggraph(dat, layout = "linear", circular = TRUE) + 
  geom_edge_arc() + 
  geom_node_point() +
  theme_graph()

ggraph(dat, layout = 'graphopt') +
  geom_edge_link() +
  geom_node_point() + 
  theme_graph()
