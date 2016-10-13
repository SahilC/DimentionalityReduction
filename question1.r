setwd('~/Code/SMAI/BayesianNets')
library(plyr)

naive_bayes_opt <- function(data,test) {
  mask <- data[,ncol(data)] == " - 50000." 
  negative <- data[mask,]
  positive <- data[!mask,]
  mistake = 0
  total = 0
  vec <- test[1,]
  data_list <- list()
  positive_list <- list()
  negative_list <- list()
  for(j in 1:(length(vec)-1)) {
    data_list[[capture.output(cat('V',j))]] <- count(data[,j])
    positive_list[[capture.output(cat('V',j))]] <- count(positive[,j])
    negative_list[[capture.output(cat('V',j))]] <- count(negative[,j])
  }
  for(i in 1:length(test)) {
    vec <- test[i,]
    predict <- test[i,length(vec)]
    pos_score = 1
    neg_score = 1
    for(j in 1:(length(vec)-1)) {
      index <- capture.output(cat('V',1))
      num_total = data_list[[index]][data_list[[index]]$x == vec[1,j],]$freq
      num_pos = positive_list[[index]][positive_list[[index]]$x == vec[1,j],]$freq
      num_neg = negative_list[[index]][negative_list[[index]]$x == vec[1,j],]$freq
      pos_score = pos_score + log(num_total/num_pos)
      neg_score = neg_score + log(num_total/num_neg)
    }
    pos_score = pos_score + log(nrow(data)/nrow(positive))
    neg_score = neg_score + log(nrow(data)/nrow(negative))
    
    if(length(pos_score) == 0) {
      naive_predict = " - 50000."
    } else {
      if((pos_score - neg_score)  >= 0.01) {
        naive_predict = " - 50000."
      } else {
        naive_predict = " 50000+."
      }
    }
    
    if(naive_predict != predict) {
      mistake = mistake + 1
    }
    total = total +1
  }
  print(mistake/total)
}
naive_bayes <- function(data, test) {
  mask <- data[,ncol(data)] == " - 50000." 
  negative <- data[mask,]
  positive <- data[!mask,]
  mistake = 0
  total = 0
  for(i in 1:100) {
    vec <- test[i,]
    predict <- test[i,length(vec)]
    pos_score = 1
    neg_score = 1
    #print(vec)
    for(j in 1:(length(vec)-1)) {
        a <- data.frame(table(data[,j]))
        b <- data.frame(table(positive[,j]))
        c <- data.frame(table(negative[,j]))
        num_total = a[a[,1] == vec[1,j],2]
        num_pos = b[b[,1] == vec[1,j],2]
        num_neg = c[c[,1] == vec[1,j],2]
        pos_score = pos_score + log(num_total/num_pos)
        neg_score = neg_score + log(num_total/num_neg)
    }
    pos_score = pos_score + log(nrow(data)/nrow(positive))
    neg_score = neg_score + log(nrow(data)/nrow(negative))

    if(length(pos_score) == 0) {
      naive_predict = " - 50000."
    } else {
      if((pos_score - neg_score)  >= 0.01) {
        naive_predict = " - 50000."
      } else {
        naive_predict = " 50000+."
      }
    }
    
    if(naive_predict != predict) {
      mistake = mistake + 1
    }
    total = total +1
  }
  print(mistake/total)
}
data <- read.table("Datasets/census-income.data",header = FALSE,stringsAsFactors=FALSE, sep = ",")
data <- data[,!sapply(data, is.numeric)]


test <- read.table("Datasets/census-income.test",header = FALSE, stringsAsFactors=FALSE, sep = ",")
test <- test[,!sapply(test, is.numeric)]
naive_bayes_opt(data,test)