#Read adjacency matrix from csv file and write it 

# Read CSV into R
adj_matrix_d7t3 <- read.csv(file="./ERGMnetworks/adj_mat1000_d7t3.csv", header=FALSE, sep=",")
m <- as.matrix(adj_matrix_d7t3)
g=graph.adjacency(m,mode="undirected",weighted=NULL) # this will create an 'igraph object'

#k <- graph_from_adjacency_matrix(adj_matrix_d7t3, mode = c("undirected"), weighted = NULL, diag = TRUE,add.colnames = NULL, add.rownames = NA)
coords = layout.fruchterman.reingold(g)
plot(g, layouts=coords, vertex.size=3, vertex.label=NA)
degree_distribution(g)



#Decompose it to giant component 
cl = clusters(g)
cl$no
table(cl$csize)
m <- decompose.graph(g)[[which(cl$csize==max(cl$csize))]]
degree_distribution(m)
mat_giant <- as_adjacency_matrix(m)
adj_mat_giant <- as.data.frame(as.matrix(mat_giant))
plot(m, layouts=coords, vertex.size=3, vertex.label=NA)
write.table(adj_mat_giant,file="./ERGMnetworks/adj_mat1000_d7t3_giant.csv", sep = ",", row.names = FALSE, col.names = FALSE)

