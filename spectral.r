adjacency_matrix <- function(data) {
    A <- matrix(0,nrow(data),nrow(data))
    for(i in 1:nrow(data)) {
        A[i,] <- apply(sweep(data,2,data[i,],`-`),1,function(x) ifelse(sqrt(sum(x**2))<=10,sqrt(sum(x**2)),0))
    }
    return(A)
}

graph_laplacian <- function(adjacency) {
    D <- diag(rowSums(A))
    D_inv <- (diag(1/rowSums(A)))** 0.5
    L <- diag(nrow(adjacency)) - D_inv %*% adjacency %*% D_inv
    return (L)
}

setwd("~/Code/SMAI/KNN")
data <- as.matrix(read.csv("Datasets/movement_libras_1.data",header = FALSE, sep = ",",fill = FALSE))
data <- data[,1:90]
A <- adjacency_matrix(data)
L <- graph_laplacian(A)

eig_vals <- eigen(L,symmetric = TRUE)

kmL <- kmeans(eig_vals$vectors[,(ncol(A)-1):(ncol(A))],centers=2,nstart=5)

segmatL <- matrix(kmL$cluster-1,45,45,byrow=T)

if(max(segmatL) & sum(segmatL==1) < sum(segmatL==0)) {
    segmatL <- abs(segmatL-1)
}

image(segmatL, col=grey((0:15)/15))
