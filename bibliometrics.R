#Work on making bibliometrix with bibliometrix package.
#Code heavily borrowed from:
# https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html

#install.packages("bibliometrix")
library(bibliometrix)
D <- readLines("savedrecs.bib")

#Convert to a data frame.
M <- convert2df(D, dbsource = "isi", format = "bibtex")

#Bibliometric analysis
results <- biblioAnalysis(M, sep = ";")
S=summary(object = results, k = 10, pause = FALSE)

#Network analysis
A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "sources", sep = ";")

# calculate jaccard similarity coefficient
S <- couplingSimilarity(NetMatrix, type="jaccard")

# plot journals' similarity (with min 3 manuscripts)
diag <- Matrix::diag
MapDegree <- 3
NETMAP <- S[diag(NetMatrix)>=MapDegree,diag(NetMatrix)>=MapDegree]
diag(NETMAP) <- 0

#Make a heatmap. 
H <- heatmap(max(NETMAP)-as.matrix(NETMAP),symm=T, cexRow=0.3,cexCol=0.3)

#install.packages("igraph") -------------------------------------------
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# define functions from package Matrix
diag <- Matrix::diag 
colSums <-Matrix::colSums

# delete not linked vertices
ind <- which(Matrix::colSums(NetMatrix)-Matrix::diag(NetMatrix)>0)
NET <- NetMatrix[ind,ind]

# Select number of vertices to plot
n <- 20    # n. of vertices
NetDegree <- sort(diag(NET),decreasing=TRUE)[n]
NET <- NET[diag(NET)>=NetDegree,diag(NET)>=NetDegree]

# delete diagonal elements (self-loops)
diag(NET) <- 0

# Create igraph object
bsk.network <- graph.adjacency(NET,mode="undirected")

# Compute node degrees (#links) and use that to set node size:
deg <- degree(bsk.network, mode="all")
V(bsk.network)$size <- deg*1.1

# Remove loops
bsk.network <- simplify(bsk.network, remove.multiple = F, remove.loops = T) 

# Choose Network layout
#l <- layout.fruchterman.reingold(bsk.network)
l <- layout.circle(bsk.network)
#l <- layout.sphere(bsk.network)
#l <- layout.mds(bsk.network)
#l <- layout.kamada.kawai(bsk.network)


## Plot the network
plot(bsk.network,layout = l, vertex.label.dist = 0.5, 
     vertex.frame.color = 'blue', vertex.label.color = 'black', 
     vertex.label.font = 1, vertex.label = V(bsk.network)$name, 
     vertex.label.cex = 0.5, main="Country collaboration")

##-----------------------------------------------------------------------------
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

# define functions from package Matrix
diag <- Matrix::diag 
colSums <-Matrix::colSums

# delete not linked vertices
ind=which(Matrix::colSums(NetMatrix)-Matrix::diag(NetMatrix)>0)
NET=NetMatrix[ind,ind]

# Select number of vertices to plot
n <- 10    # n. of vertices
NetDegree <- sort(diag(NET),decreasing=TRUE)[n]
NET <- NET[diag(NET)>=NetDegree,diag(NET)>=NetDegree]

# delete diagonal elements (self-loops)
diag(NET) <- 0

# Create igraph object
bsk.network <- graph.adjacency(NET,mode="undirected")

# Remove loops
bsk.network <- simplify(bsk.network, remove.multiple = F, remove.loops = T) 

# Choose Network layout
l = layout.fruchterman.reingold(bsk.network)

## Plot
plot(bsk.network,layout = l, vertex.label.dist = 0.5, vertex.frame.color = 'blue', 
     vertex.label.color = 'black', vertex.label.font = 1, 
     vertex.label = V(bsk.network)$name, vertex.label.cex = 0.5, 
     main="Co-citation network")


#------------------------------------------------------------------------------
#Keyword coupling
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "keywords", sep = ";")

# define functions from package Matrix
diag <- Matrix::diag 
colSums <-Matrix::colSums

# delete not linked vertices
ind=which(Matrix::colSums(NetMatrix)-Matrix::diag(NetMatrix)>0)
NET=NetMatrix[ind,ind]

# Select number of vertices to plot
n <- 10    # n. of vertices
NetDegree <- sort(diag(NET),decreasing=TRUE)[n]
NET <- NET[diag(NET)>=NetDegree,diag(NET)>=NetDegree]

# delete diagonal elements (self-loops)
diag(NET) <- 0

# Plot Keywords' Heatmap (most frequent 30 words)
n=30
NETMAP=NetMatrix[ind,ind]
MapDegree <- sort(diag(NETMAP),decreasing=TRUE)[n]
NETMAP <- NETMAP[diag(NETMAP)>=MapDegree,diag(NETMAP)>=MapDegree]
diag(NETMAP) <- 0

H <- heatmap(max(NETMAP)-as.matrix(NETMAP),symm=T, cexRow=0.3,cexCol=0.3)


# Create igraph object
bsk.network <- graph.adjacency(NET,mode="undirected")

# Remove loops
bsk.network <- simplify(bsk.network, remove.multiple = T, remove.loops = T) 

# Choose Network layout
l = layout.fruchterman.reingold(bsk.network)


## Plot
plot(bsk.network,layout = l, vertex.label.dist = 0.5, 
     vertex.frame.color = 'black', vertex.label.color = 'black',
     vertex.label.font = 1, vertex.label = V(bsk.network)$name, 
     vertex.label.cex = 0.5, main="Keyword coupling")

#####--------------------------------------------------------------------------
#Co-word analysis: conceptual structure of a field - 
#this example uses MCA (Multiple Correspondence Analysis) and k-means clusterin.
# Create a bipartite network of Keyword plus
#
# each row represents a manuscript
# each column represents a keyword (1 if present, 0 if absent in a document)

CW <- cocMatrix(M, Field = "ID", type="matrix", sep=";")
# dimension of CW
dim(CW)

# Define minimum degree (number of occurrences of each Keyword)
Degree=5
CW=CW[,colSums(CW)>=Degree]

# Delete empty rows
CW=CW[rowSums(CW)>0,]

# Dimension of Data matrix
dim(CW)

# Recode as dataframe
CW=data.frame(apply(CW,2,factor))

# Delete not consistent keywords
names(CW)
CW = CW[,-3]
#install.packages("FactoMineR")
library(FactoMineR)
#install.packages("factoextra")
library(factoextra)

# Perform Multiple Correspondence Analysis (MCA)
res.mca <- MCA(CW, ncp=2, graph=FALSE)

# Get coordinates of keywords (we take only categories "1"")
coord=get_mca_var(res.mca)
df=data.frame(coord$coord)[seq(2,dim(coord$coord)[1],by=2),]
row.names(df)=gsub("_1","",row.names(df))

# K-means clustering

# Selection of optimal number of clusters (silhouette method)
fviz_nbclust(scale(df), kmeans, method = "silhouette")
#Figure shows us that 2 clusters is optimal.

# Perform the K-means clustering.
library(RColorBrewer) #for figure.
#set the number of clusters as argument 2 in the following line: 
km.res <- kmeans(scale(df), 6, nstart = 25)

# Plot of the conceptual map
fviz_cluster(km.res, data = df,labelsize=2)+theme_minimal()+
  scale_colour_brewer(type=qual, palette = "Dark2")+
  #scale_color_manual(values = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))+
  #scale_fill_manual(values = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07")) +
  labs(title= "     ") +
  geom_point()


