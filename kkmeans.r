construct_kernel <- function(data_in,data_out,sequence,gamma = 0.00000002) {
      K = matrix(0,nrow(data_in),length(sequence))
  for (i in 1:nrow(data_in)) {
          for (j in 1:length(sequence)) {
                    K[i,j] <- exp(-1*gamma*sum((data_in[i,] - data_out[sequence[j],])**2,na.rm = T))
      }    
    }
    return(K)
}

project_points <- function(data_in,data_out,vectors) {
      sequence = 1:nrow(data_out)
  K <- construct_kernel(data_in,data_out,sequence)
    return (t(K) %*% vectors)
}

kernel_transform <- function(data) {
      sequence = 1:nrow(data)
  print("Constructing Kernel")
    K <- construct_kernel(data,data,sequence)
    I <- matrix(1,nrow(data),nrow(data))/nrow(data)
      K_centered <- K - I%*%K - K%*%I + I%*%K%*%I
      eig <- eigen(K_centered)
        vectors <- as.matrix(eig$vectors[,1:10])/ sqrt(eig$values[1:10])
        # final_data <- t(t(vectors) %*% t(X))
        # return(final_data)
        return(vectors)
}

data <- as.matrix(read.csv("Datasets/movement_libras_1.data",header = FALSE, sep = ",",fill = FALSE))
vect <- kernel_transform(data)
a <- project_points(data,data,vect)

kmL <- kmeans(a,centers=3,nstart=5)
