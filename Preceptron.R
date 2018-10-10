# Author : Muhammad Bima Adi Prabowo
# File   : Preceptron

#Dataset Pola Huruf
A1 <- read.csv("A1.csv", sep = ";", header = FALSE)
A1 <- as.matrix(A1)
A1 <- matrix(data = A1, nrow = 1, byrow = TRUE)

A2 <- read.csv("A2.csv", sep = ";", header = FALSE)
A2 <- as.matrix(A2)
A2 <- matrix(data = A2, nrow = 1, byrow = TRUE)

B1 <- read.csv("B1.csv", sep = ";", header = FALSE)
B1 <- as.matrix(B1)
B1 <- matrix(data = B1, nrow = 1, byrow = TRUE)

B2 <- read.csv("B2.csv", sep = ";", header = FALSE)
B2 <- as.matrix(B2)
B2 <- matrix(data = B2, nrow = 1, byrow = TRUE)

C1 <- read.csv("C1.csv", sep = ";", header = FALSE)
C1 <- as.matrix(C1)
C1 <- matrix(data = C1, nrow = 1, byrow = TRUE)

C2 <- read.csv("C2.csv", sep = ";", header = FALSE)
C2 <- as.matrix(C2)
C2 <- matrix(data = C2, nrow = 1, byrow = TRUE)

masukanPola <- rbind(A1, A2, B1, B2, C1, C2)
# 1 pola A, -1 bukan pola A
targetPola <- c(1, 1, -1, -1, -1, -1)

#dataset AND
a <- c(1, 1)
b <- c(1, -1)
c <- c(-1, 1)
d <- c(-1, -1)
masukanAND <- rbind(a, b, c, d)
targetAND <- c(1, -1, -1, -1)

#Output weight(bobot) in vector
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
        print("NOT konvergen")
        bobot <- bobot + alpha * target[i] * masukan[i, ]
      }
    }
    currentEpoch <- currentEpoch + 1
  }
  
  return(bobot)
}

#Input testCase = testCase, result = weight/bobot
#Output 1 = Yes, 0 = Between, -1 = No
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

resultPola <- preceptron(masukan = masukanPola, target = targetPola)
resultAND <- preceptron(masukan = masukanAND, target = targetAND)

print(testPreceptron(testCase = a, result = resultAND))
print(testPreceptron(testCase = A1, result = resultPola))
