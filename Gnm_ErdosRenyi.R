## Download and install the package
install.packages("igraph")

## Load package
library(igraph)
g <- sample_gnm(10, 7)
degree_distribution(g)
mat_10 <- as_adjacency_matrix(g)
adj_mat <- as.data.frame(as.matrix(mat_10))

coords = layout.fruchterman.reingold(g)
plot(g, layouts=coords, vertex.size=3, vertex.label=NA)
cl = clusters(g)
cl$no
table(cl$csize)
q <- decompose.graph(g)[[which(cl$csize==max(cl$csize))]]
mat_giant <- as_adjacency_matrix(q)
adj_mat_giant <- as.data.frame(as.matrix(mat_giant))
plot(q, layouts=coords, vertex.size=3, vertex.label=NA)
#giant.component.extract(g, directed = FALSE, bipartite.proj = FALSE, num.proj = 1)

write.table(adj_mat,file="./ERGMnetworks/adj_mat1000_giant_m6000.csv", sep = ",", row.names = FALSE, col.names = FALSE)

