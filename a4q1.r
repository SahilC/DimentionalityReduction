construct_kernel <- function(data,sequence) {
  K = matrix(0,nrow(data),length(sequence))
  for (i in 1:nrow(data)) {
    for (j in 1:length(sequence)) {
      K[i,j] <- exp(-0.00000002*sum((data[i,] - data[sequence[j],])**2,na.rm = T))
    }    
  }
  return(K)
}

kernel_principle_components <- function(data) {
  sequence = 1:nrow(data)
  K <- construct_kernel(data,sequence)
  I <- diag(nrow(data))/nrow(data)
  K_centered <- K - I%*%K - K%*%I + I%*%K%*%I
  eig <- eigen(K_centered)
  vectors <- as.matrix(eig$vectors[,1:20])
  # final_data <- t(t(vectors) %*% t(X))
  # return(final_data)
  return(vectors)
}

kernel_linear_discriminant <- function(data,labels) {
  A <- data
  A <- as.matrix(cbind(A, labels))
  
  negative_sequence <- which(A[,ncol(A)] == -1)
  K_negative <- construct_kernel(data,negative_sequence)
  num_negative <- length(negative_sequence)
  K_negative <- K_negative %*% (diag(num_negative) - (1/num_negative)*matrix(1,num_negative,num_negative)) %*% t(K_negative)
  
  positive_sequence <- which(A[,ncol(A)] == 1)
  K_positive <- construct_kernel(data,positive_sequence)
  num_positive <- length(positive_sequence)
  K_positive <- K_positive %*% (diag(num_positive) - (1/num_positive)*matrix(1,num_positive,num_positive)) %*% t(K_positive)
  
  M1 <- apply(K_positive,1,function(X) (1/length(X))*sum(X))
  M2 <- apply(K_negative,1,function(X) (1/length(X))*sum(X))
  
  N <- K_positive + K_negative
  
  N <- N + 5*diag(100)
  
  N_inv <- solve(N)
  
  vect <- N_inv %*% (M2-M1)
  # final_data <- t(t(vectors) %*% t(X))
  # return(final_data)
  return(vect)
}

data <- as.matrix(read.csv("Datasets/arcene_train.data",header = FALSE, sep = " ",fill = FALSE))
labels <- read.csv("Datasets/arcene_train.labels", header = FALSE, sep = " ") 

test <- read.csv("Datasets/arcene_valid.data", header = FALSE, sep = " ",  fill = TRUE)
test_labels <- read.csv("Datasets/arcene_valid.labels", header = FALSE, sep = " ")
v <- kernel_linear_discriminant(data,labels)