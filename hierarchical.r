setwd('~/Code/SMAI/KNN')
manhattan_distance <- function(p1,p2) {
  return(abs(sum(p1 - p2)))
}

squared_euclidean_distance <- function(p1,p2) {
  return(sum((p1 - p2)**2))
}

get_centroid <- function(l) {
  if(length(l) > 91) {
    v <- apply(l,2,function(x) sum(x))
    v <- v/nrow(l)
  } else {
    v <- l
  }
  return(v)
}

merge_clusters <- function(clusters,min_i,val_i,val_j) {
  for(i in 1:length(clusters)) {
    if(clusters[[i]] == val_i || clusters[[i]] == val_j) {
      clusters[[i]] <- min_i   
    }
  }
  return(clusters)
}

cluster_centroids <- function(data,clusters) {
  reduced_set <- unique(clusters)
  min_val = squared_euclidean_distance(data[[1]],data[[2]])
  min_i = 1
  min_val_i = reduced_set[[1]]
  min_val_j = reduced_set[[2]]
  for(i in 1:length(data)) {
    for(j in i:length(data)) {
      if(i != j) {
      distance = squared_euclidean_distance(data[[i]],data[[j]])
      if(distance < min_val) {
        min_val = distance
        min_i = i
        min_val_i = reduced_set[[i]]
        min_val_j = reduced_set[[j]]
      }
      }
    }
  }
  clusters <- merge_clusters(clusters,min_i,min_val_i,min_val_j)
  return(clusters)
}

get_min_distance <- function(c1,c2) {
  min_val = squared_euclidean_distance(c1[[1]],c2[[1]])
  for(i in 1:length(c1)) {
    for(j in 1:length(c2)) {
      distance = squared_euclidean_distance(c1[[i]],c2[[j]])
      if(distance <= min_val) {
        min_val = distance
      }
    }
  }
  return(min_val)
}

cluster_min <- function(data,clusters) {
  reduced_set <- unique(clusters)
  min_i = 1
  min_val_i = reduced_set[[1]]
  min_val_j = reduced_set[[2]]
  min_val <- 99999
  for(i in reduced_set) {
    for(j in reduced_set) {
      if(i != j) {
      c1 <- data[clusters == i,]
      c2 <- data[clusters == j,]
      distance <- get_min_distance(c1,c2)
      if(distance < min_val) {
        min_val = distance
        min_i = i
        min_val_i = reduced_set[[i]]
        min_val_j = reduced_set[[j]]
      }
      }
    }
  }
  print(min_val_i)
  print(min_val_j)
  clusters <- merge_clusters(clusters,min_i,min_val_i,min_val_j)
  return(clusters)
}

data <- as.matrix(read.csv("Datasets/movement_libras_1.data",header = FALSE, sep = ",",fill = FALSE))
clusters <- vector('integer')
for(i in 1:45) {
  #mat[[i]] <- data[i,]
  clusters[[i]] <- i
}
#clusters <- cluster(mat,clusters)
while(length(unique(clusters)) > 1) {
  mat <- list()
  val <- unique(clusters)
  # for(i in 1:length(val)) {
  #   mat[[i]] <- get_centroid(data[clusters == val[i],])
  # }
  print(length(val))
  # clusters <- cluster_centroids(mat,clusters)
  clusters <- cluster_min(data,clusters)
}

