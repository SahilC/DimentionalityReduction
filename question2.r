setwd('~/Code/SMAI/BayesianNets')

dense_to_sparse <- function(data,nfeatures) {
  A = matrix(0,nrow(data),nfeatures)
  for (k in 1:dim(data)[1]) {
    for (i in 1:(dim(data)[2])) {
      if(!is.na(data[k,i]) && data[k,i]<= nfeatures) {
        A[k,data[k,i]] <- 1
      }
    }
  }
  return(A)
}

fast_dense_to_sparse <- function(data,nfeatures) {
  A = matrix(0,nrow(data),nfeatures)
  for (k in 1:dim(data)[1]) {
    # for (i in 1:(dim(data)[2])) {
    #   if(!is.na(data[k,i]) && data[k,i]<= nfeatures) {
    #     A[k,data[k,i]] <- 1
    #   }
    # }
    A[k,data[k,] <= nfeatures] <- 1
  }
  return(A)
}
principle_components <- function(data) {
  # A <- dense_to_sparse(data,2000)
  A <- data
  X <- as.matrix(apply(A, 2, function(y) y - mean(y)))
  Y <- cov(X)
  eig <- eigen(Y)
  vectors <- as.matrix(eig$vectors[,1:20])
  # final_data <- t(t(vectors) %*% t(X))
  # return(final_data)
  return(vectors)
}

fisher_components <- function(data, labels) {
  # A <- dense_to_sparse(data,200)
  A <- data
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
  vectors <- as.matrix(eig$vectors[,1:1])
  A[,ncol(A)] <- NULL
  # final_data <- t(t(vectors) %*% t(A))
  # return(final_data)
  return(Re(vectors))
}

gaussian_naive_bayes <- function(data, test,test_labels) {
  mask <- data[,ncol(data)] == -1 
  negative <- data[mask,]
  positive <- data[!mask,]
  positive[,ncol(positive)] <- NULL
  negative[,ncol(negative)] <- NULL
  
  Mu1 <- apply(positive, 2, function(y) mean(y))
  Mu2 <- apply(negative, 2, function(y) mean(y))
  
  Var1 <- apply(positive, 2, function(y) var(y))
  Var2 <- apply(negative, 2, function(y) var(y))
  
  A <- t(as.matrix(apply(test, 1, function(y) y - Mu1)))
  B <- t(as.matrix(apply(test, 1, function(y) y - Mu2)))
  
  A <- A^2
  B <- B^2
  
  Var1 <- 2*Var1^2
  Var2 <- 2*Var2^2
  
  C <- exp(t(as.matrix(apply(A, 1, function(y) -1*y/Var1))))
  D <- exp(t(as.matrix(apply(B, 1, function(y) -1*y/Var2))))
  
  # U <- t(as.matrix(apply(C,1, function(y) (1/sqrt(pi*Var1))*y)))
  # V <- t(as.matrix(apply(D,1, function(y) (1/sqrt(pi*Var1))*y)))
  
  mistake = 0
  total = 0
  for(i in 1:nrow(test)) {
    predict <- test_labels[i,]
    vec <- C[i,]
    vec2 <- D[i,]
    pos_score <- sum(sapply(vec,function(x) log(x+1)))
    neg_score <- sum(sapply(vec2,function(x) log(x+1)))
    #predict <- test[i,length(vec)]
    pos_score = log(nrow(positive)/nrow(data)) + pos_score
    neg_score = log(nrow(negative)/nrow(data)) + neg_score
    if((neg_score - pos_score)  >= 0.01) {
        naive_predict = -1
    } else {
        naive_predict = 1
    }
    
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
# vectors <- fisher_components(data,labels)
# vectors <- principle_components(data)
test <- read.table("Datasets/dorothea_valid.data", header = FALSE, sep = " ", col.names = paste0("V",seq_len(6100)), fill = TRUE)
test_labels <- read.table("Datasets/dorothea_valid.labels", header = FALSE, sep = " ")

ddata <- dense_to_sparse(data,200)
dtest <- dense_to_sparse(test,200)
fdata <- t(t(vectors) %*% t(ddata))
fdata <- cbind(fdata, labels)
ftest <- t(t(vectors) %*% t(dtest))
# ftest <- cbind(test, test_labels)


