install.packages('statnet')
library(statnet)

## Download and install the package
install.packages("igraph")

## Load package
library(igraph)


#MODEL WITH  triads
numNodes <- 100
avgDegree <- 3
avgTriads <- 1
#Clustering Coefficient = avgTriads / ((avgDegree * (avgDegree - 1))/2)
triadModel.net <- network.initialize(numNodes, directed=F)
triadModel.edges <- (avgDegree * numNodes) / 2 
triadModel.triangle <- (avgTriads * numNodes) 
summary(triadModel.net)
triadModel.target.stats <- c(triadModel.edges, triadModel.triangle)
triadModel.fit <- ergm(triadModel.net ~ edges + gwesp(0.25,fixed=T) , target.stats = triadModel.target.stats)
summary(triadModel.fit)
triadModel.sim1 <- simulate(triadModel.fit)
summary(triadModel.sim1 ~ edges + triangles) 
adj_mat <- triadModel.sim1[,]
write.table(adj_mat,file="./ERGMnetworks/adj_mat1000_d8t45.csv", sep = ",", row.names = FALSE, col.names = FALSE)

#Create graph from adjacency matrix 
k <- graph_from_adjacency_matrix(adj_mat, mode = c("undirected"), weighted = NULL, diag = TRUE,
                            add.colnames = NULL, add.rownames = NA)
coords = layout.fruchterman.reingold(k)
plot(k, layouts=coords, vertex.size=3, vertex.label=NA)
degree_distribution(k)

#Decompose it to giant component 
cl = clusters(k)
cl$no
table(cl$csize)
m <- decompose.graph(k)[[which(cl$csize==max(cl$csize))]]
mat_giant <- as_adjacency_matrix(m)
adj_mat_giant <- as.data.frame(as.matrix(mat_giant))
plot(m, layouts=coords, vertex.size=3, vertex.label=NA)
