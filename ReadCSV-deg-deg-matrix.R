## Download and install the package
install.packages("igraph")
## Load package
library(igraph)

################ Read adjacency matrix in .csv file 
my_data <- read.csv(file="./ERGMnetworks/adj_mat15_d5t2.csv", header = FALSE, sep = ",")
adj_mat_giant <- my_data

######## Find degree of 2nd degree contacts (Conditional degree)
degrees_of_nodes <- apply(adj_mat_giant,2,sum)
max_degree <- max(degrees_of_nodes)
L <- matrix(, nrow = max_degree, ncol = max_degree)
L[is.na(L)] <- 0

mat_giant <- adj_mat_giant

# Loop over mat_giant 
for(row in 1:nrow(mat_giant)) {
  deg <- degrees_of_nodes[row] #degree of the corresponding node
  #print(deg)
  contacts <- which(mat_giant[row,]==1) #contacts of the corresponding node
  #print(contacts)
  for(i in 1:length(contacts)) {
    contact <- contacts[i] 
    deg_contact <- degrees_of_nodes[contact] #degree of the contact
    #print(deg_contact)
    L[deg, deg_contact] = L[deg, deg_contact] + 1
    
  }
  print(L)
}
print(L)

write.table(L,file="./ConditionalProb/adj_mat1000_m3500_degree.csv", sep = ",", row.names = FALSE, col.names = FALSE)

#find conditional probability by dividing each element in column by its sum
conditional_prob <- apply(L, 2, function(i) i/sum(i))
print(conditional_prob)
conditional_prob[is.na(conditional_prob)] <- 0

write.table(conditional_prob,file="./ConditionalProb/adj_mat1000_m3500.csv", sep = ",", row.names = FALSE, col.names = FALSE)

# par(oma=c(4,4,2,2))
# par(mfrow = c(max_degree, 1))
# #par(mar=c(1,1,1,1))
# par(mai=c(0.2,0.4,0.2,0))
# par(mgp = c(1.5, 0.5, 0))
# for (i in 1:max_degree){
#   plot(conditional_prob[,i], type = "l", ylab = paste("degree ", i), xlab = "degree")
# }
# 
