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
