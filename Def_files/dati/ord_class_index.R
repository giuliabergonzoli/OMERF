# input : confusion matrix and number of classes
# size ( cMatrix ) must be [K K] # table with the true class in rows and the predicted class in columns
OrdinalClassificationIndex <- function(cMatrix, K) {
  N <- sum(cMatrix)
  ggamma <- 1
  bbeta <- 0.75 / (N * (K - 1) ^ ggamma)
  
  helperM2 <- matrix(0, nrow = K, ncol = K)
  for (r in 1:K) {
    for (c in 1:K) {
      helperM2[r, c] <- cMatrix[r, c] * (abs(r - c) ^ ggamma)
    }
  }
  
  TotalDispersion <- (sum(helperM2) ^ (1 / ggamma))
  helperM1 <- cMatrix / (TotalDispersion + N)
  
  errMatrix <- matrix(0, nrow = K, ncol = K)
  errMatrix[1, 1] <- 1 - helperM1[1, 1] + bbeta * helperM2[1, 1]
  
  for (r in 2:K) {
    for (c in 1) {
      errMatrix[r, c] <- errMatrix[r - 1, c] - helperM1[r, c] + bbeta * helperM2[r, c]
    }
  }
  
  for (c in 2:K) {
    for (r in 1) {
      errMatrix[r, c] <- errMatrix[r, c - 1] - helperM1[r, c] + bbeta * helperM2[r, c]
    }
  }
  
  for (c in 2:K) {
    for (r in 2:K) {
      costup <- errMatrix[r - 1, c]
      costleft <- errMatrix[r, c - 1]
      lefttopcost <- errMatrix[r - 1, c - 1]
      aux <- min(c(costup, costleft, lefttopcost))
      errMatrix[r, c] <- aux - helperM1[r, c] + bbeta * helperM2[r, c]
    }
  }
  
  oc <- errMatrix[K, K]
  return(oc)
}
