setwd('~/Code/SMAI/BayesianNets')

dense_to_sparse <- function(data,nfeatures) {
  A = matrix(0,800,nfeatures)
  for (k in 1:dim(data)[1]) {
    for (i in 1:(dim(data)[2])) {
      if(!is.na(data[k,i]) && data[k,i]<= nfeatures) {
        A[k,data[k,i]] <- 1
      }
    }
  }
  return(A)
}

principle_components <- function(data) {
  A <- dense_to_sparse(data,2000)
  X <- as.matrix(apply(A, 2, function(y) y - mean(y)))
  Y <- cov(X)
  eig <- eigen(Y)
  vectors <- as.matrix(eig$vectors[,1:50])
  final_data <- t(t(vectors) %*% t(X))
  return(final_data)
}

fisher_components <- function(data, labels) {
  A <- dense_to_sparse(data,200)
  A <- cbind(A, labels)
  X <- A[A$V1 == 1,]
  Y <- A[A$V1 == -1,]
  X[,201] <- NULL
  Y[,201] <- NULL
  
  Mu1 <- as.matrix(apply(X, 2, function(y) mean(y)))
  Mu2 <- as.matrix(apply(Y, 2, function(y) mean(y)))
  
  U <- X - Mu1
  V <- Y - Mu2
  
  S1 <- cov(U)
  S2 <- cov(V)
  SW <- S1 + S2
  SB <- (Mu1 - Mu2) %*% t(Mu1-Mu2)
  invSw <- solve(SW) %*% SB
  eig <- eigen(invSw)
  vectors <- as.matrix(eig$vectors[,1:50])
  A[,ncol(A)] <- NULL
  final_data <- t(t(vectors) %*% t(A))
  return(final_data)
}

gaussian_naive_bayes <- function(data, test) {
  mask <- data[,ncol(data)] == -1 
  negative <- data[mask,]
  positive <- data[!mask,]
  positive[,ncol(positive)+1] <- NULL
  negative[,ncol(negative)+1] <- NULL
  
  Mu1 <- as.matrix(apply(positive, 2, function(y) mean(y)))
  Mu2 <- as.matrix(apply(negative, 2, function(y) mean(y)))
  
  Var1 <- as.matrix(apply(positive, 2, function(y) var(y)))
  Var2 <- as.matrix(apply(negative, 2, function(y) var(y)))
  mistake = 0
  total = 0
  for(i in 1:100) {
    if(naive_predict != predict) {
      mistake = mistake + 1
    }
    total = total +1
  }
  print(mistake/total)
}

data <- read.table("Datasets/dorothea_train.data", header = FALSE, sep = " ", col.names = paste0("V",seq_len(6100)), fill = TRUE)
labels <- read.table("Datasets/dorothea_train.labels", header = FALSE, sep = " ") 
#principle_components(data)
final_data <- fisher_components(data,labels)

