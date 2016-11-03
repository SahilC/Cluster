setwd('~/Code/SMAI/KNN')
manhattan_distance <- function(p1,p2) {
  return(sum(p1 - p2))
}

squared_euclidean_distance <- function(p1,p2) {
  return(sum((p1 - p2)**2))
}

get_centroid <- function(l) {
  return(Reduce("+",l)/length(l))
}

cluster <- function(data,clusters) {
  min_val = manhattan_distance(data[[1]],data[[2]])
  min_i = 1
  min_j = 2
  for(i in 1:length(data)) {
    for(j in 1:length(data)) {
      if( i != j) {
          distance = manhattan_distance(data[[i]],data[[j]])
          if(distance < min_val) {
            min_val = distance
            min_i = i
            min_j = j
          }
      }
    }
  }
  
  clusters[[min_i]] <- min_i 
  clusters[[min_j]] <- min_i 
  return(clusters)
}

data <- as.matrix(read.csv("Datasets/movement_libras_1.data",header = FALSE, sep = ",",fill = FALSE))
clusters <- list()
for(i in 1:500) {
  #mat[[i]] <- data[i,]
  clusters[[i]] <- i
}
#clusters <- cluster(mat,clusters)
while(length(unique(clusters)) > 1) {
  mat <- list()
  val <- unique(clusters)
  for(i in 1:length(val)) {
    clust <- list()
    k = 1
    for(j in 1:length(clusters)) {
      if(clusters[[j]] == val[i]) {
        clust[[k]] <- data[j,]
        k = k + 1
      }
    }
    mat[[i]] <- get_centroid(clust)
  }
  clusters <- cluster(mat,clusters)
}

