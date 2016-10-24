construct_kernel <- function(data) {
  K = matrix(0,nrow(data),nrow(data))
  for (i in 1:nrow(data)) {
    for (j in 1:nrow(data)) {
      K[i,j] <- exp(-0.00000002*sum((data[i,] - data[j,])**2,na.rm = T))
    }    
  }
  return(K)
}

kernel_principle_components <- function(data) {
  K <- construct_kernel(data)
  I <- diag(5)/nrow(data)
  K_centered <- K - I%*%K - K%*%I + I%*%K%*%I
  eig <- eigen(K_centered)
  vectors <- as.matrix(eig$vectors[,1:20])
  # final_data <- t(t(vectors) %*% t(X))
  # return(final_data)
  return(vectors)
}

data <- as.matrix(read.csv("Datasets/arcene_train.data", sep = " ",fill = FALSE))
labels <- read.csv("Datasets/arcene_train.labels", header = FALSE, sep = " ") 

test <- read.csv("Datasets/arcene_valid.data", header = FALSE, sep = " ",  fill = TRUE)
test_labels <- read.csv("Datasets/arcene_valid.labels", header = FALSE, sep = " ")