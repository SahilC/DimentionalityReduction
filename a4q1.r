setwd('~/Code/SMAI/BayesianNets')
library(e1071)

norm_vec <- function(x) sqrt(sum(x^2))

construct_kernel <- function(data_in,data_out,sequence,gamma = 0.00000002) {
  K = matrix(0,nrow(data),length(sequence))
  for (i in 1:nrow(data)) {
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

kernel_principle_components <- function(data) {
  sequence = 1:nrow(data)
  K <- construct_kernel(data,data,sequence)
  I <- matrix(1,nrow(data),nrow(data))/nrow(data)
  K_centered <- K - I%*%K - K%*%I + I%*%K%*%I
  eig <- eigen(K_centered)
  vectors <- as.matrix(eig$vectors[,1:50])/ sqrt(eig$values[1:50])
  # final_data <- t(t(vectors) %*% t(X))
  # return(final_data)
  return(vectors)
}

kernel_linear_discriminant <- function(data,labels) {
  A <- data
  A <- as.matrix(cbind(A, labels))
  
  negative_sequence <- which(A[,ncol(A)] == -1)
  K_negative <- construct_kernel(data,data,negative_sequence)
  M2 <- apply(K_negative,1,function(X) (1/length(X))*sum(X))
  
  num_negative <- length(negative_sequence)
  K_negative <- K_negative %*% (diag(num_negative) - (1/num_negative)*matrix(1,num_negative,num_negative)) %*% t(K_negative)
  
  positive_sequence <- which(A[,ncol(A)] == 1)
  K_positive <- construct_kernel(data,data,positive_sequence)
  M1 <- apply(K_positive,1,function(X) (1/length(X))*sum(X))
  
  num_positive <- length(positive_sequence)
  K_positive <- K_positive %*% (diag(num_positive) - (1/num_positive)*matrix(1,num_positive,num_positive)) %*% t(K_positive)
  
  N <- K_positive + K_negative
  
  #print(dim(N))
  N <- N + diag(100)
  
  N_inv <- solve(N)
  
  vect <- N_inv %*% (M2-M1)
  # final_data <- t(t(vectors) %*% t(X))
  # return(final_data)
  return(vect)
}

run_KPCA <- function(data, labels,test,test_labels) {
  #vect <- kernel_linear_discriminant(data,labels)
  vect <- kernel_principle_components(data)
  
  a <- project_points(data,data,vect)
  #mysvm <- svm(a,labels,type='C',kernel = 'radial',gamma=0.1,cost = 100)
  mysvm <- svm(a,labels,type='C',kernel = 'radial',gamma= 0.01,cost = 10)
  b <- project_points(data,test,vect)
  pred <- predict(mysvm,b)
  length(which(as.vector(pred) == test_labels))
}

run_KLDA <- function(data,labels,test,test_labels) {
  vect <- kernel_linear_discriminant(data,labels)
  
  a <- project_points(data,data,vect)
  #mysvm <- svm(a,labels,type='C',kernel = 'radial',gamma=0.1,cost = 100)
  mysvm <- svm(a,labels,type='C',kernel = 'radial',gamma= 0.01,cost = 10)
  b <- project_points(data,test,vect)
  pred <- predict(mysvm,b)
  length(which(as.vector(pred) == test_labels))
}


# data <- as.matrix(read.csv("Datasets/arcene_train.data",header = FALSE, sep = " ",fill = FALSE))
# labels <- read.csv("Datasets/arcene_train.labels", header = FALSE, sep = " ") 
# 
# test <- as.matrix(read.csv("Datasets/arcene_valid.data", header = FALSE, sep = " ",  fill = TRUE))
# test_labels <- read.csv("Datasets/arcene_valid.labels", header = FALSE, sep = " ")
# run_KPCA(data,labels,test,test_labels)
# run_KLDA(data,labels,test,test_labels)
