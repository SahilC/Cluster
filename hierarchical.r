setwd('~/Code/SMAI/KNN')
manhattan_distance <- function(p1,p2) {
  return(sum(p1 - p2))
}

squared_euclidean_distance <- function(p1,p2) {
  return(sum((p1 - p2)**2))
}

cluster <- function(data) {
  min_val = manhattan_distance(data[0,],data[1,])
  min_i = 0
  min_j = 1
  for(i in 1:nrow(data)) {
    for(j in 1:nrow(data)) {
      if( i != j) {
          distance = manhattan_distance(data[i,],data[j,])
          if(distance < min_val) {
            min_val = distance
            min_i = i
            min_j = j
          }
      }
    }
  }
}

data <- as.matrix(read.csv("Datasets/movement_libras_1.data",header = FALSE, sep = ",",fill = FALSE))