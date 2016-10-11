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
  # final_data <- t(t(vectors) %*% t(A))
  # return(final_data)
  return(vectors)
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
  
  U <- (X - Mu1)^2
  V <- (Y - Mu2)^2
  
  Var1 <- 2*Var1^2
  Var2 <- 2*Var2^2
  
  U <- -1*U/Var1
  V <- -1*V/Var2
  
  U <- exp(U)
  V <- exp(V)
  
  U <- (1/sqrt(pi*Var1))*U
  V <- (1/sqrt(pi*Var2))*V
  
  mistake = 0
  total = 0
  for(i in 1:100) {
    vec <- test[i,]
    #predict <- test[i,length(vec)]
    pos_score = 1
    neg_score = 1
    #print(vec)
    for(j in 1:(length(vec)-1)) {
      pos_score = pos_score + log()
      neg_score = neg_score + log(num_total/num_neg)
    }
    pos_score = pos_score + log(nrow(data)/nrow(positive))
    neg_score = neg_score + log(nrow(data)/nrow(negative))
    
    if(length(pos_score) == 0) {
      naive_predict = -1
    } else {
      if((pos_score - neg_score)  >= 0.01) {
        naive_predict = -1
      } else {
        naive_predict = 1
      }
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
vectors <- fisher_components(data,labels)
test <- read.table("Datasets/dorothea_valid.data", header = FALSE, sep = " ", col.names = paste0("V",seq_len(6100)), fill = TRUE)
test <- dense_to_sparse(test,200)

