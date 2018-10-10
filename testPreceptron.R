a <- c(1, 1)
b <- c(1, -1)
c <- c(-1, 1)
d <- c(-1, -1)
masukan <- rbind(a, b, c, d)
target <- c(1, -1, -1, -1)

#fungsi preceptron
#preceptron <- function(masukan, target, maxEpoch = 20, alpha = 1)
preceptron <- function(masukan, target, maxEpoch = 20, alpha = 1){
  #tambah bias (1)
  masukan <- cbind(masukan, rep(1, nrow(masukan)))
  bobot <- rep(x = 0, ncol(masukan))
  
  konvergen <- FALSE
  currentEpoch <- 0
  while (konvergen == FALSE && currentEpoch != maxEpoch) {
    konvergen <- TRUE
    # 1 Epoch
    for(i in 1:nrow(masukan)){
      keluaran <- sum(masukan[i, 1:ncol(masukan) - 1] * bobot[1:length(bobot) - 1]) + bobot[length(bobot)]
      if(keluaran > 0){
        keluaran <- 1
      }else if(keluaran == 0){
        keluaran <- 0
      }else{
        keluaran <- -1
      }
      print(bobot)
      print(keluaran)
      if(keluaran != target[i]){
        konvergen <- FALSE
        print("belum konvergen")
        # for(j in 1:length(bobot)){
        #   bobot[j] <- bobot[j] + alpha*target[i]*masukan[i, j]
        # }
        bobot <- bobot + alpha * target[i] * masukan[i, ]
      }
    }
    currentEpoch <- currentEpoch + 1
  }
  
  return(bobot)
}
result <- preceptron(masukan, target)

testPreceptron <- function(testCase, result){
  net <- sum(testCase[1:length(testCase)] * result[1:length(result) - 1]) + result[length(result)] 
  if(net > 0 ){
    return(1)
  }else if(net == 0){
    return(0)
  }else{
    return(-1)
  }  
}
print(testPreceptron(d, result))

