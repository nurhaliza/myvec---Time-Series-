library(tidyverse)
load("~/Users/nurhalizahassan/Desktop/myvec---Time-Series-/myvec.RData")

acf(myvec, 500)
ts <- ts(myvec, frequency = 50)
plot(decompose(ts))
A <- tibble("x" = c(2, 0, 2, 4), "y" = c(2, 4, 6, 5))
B <- tibble("x" = c(1, 0, 4), "y" = c(1, 6, 4))

# dtw function
dtw <- function (A, B) {
  M <- nrow(A)
  N <- nrow(B)
  Cost <- matrix(0,M,N) # Initialize with zeros
  for (i in 1:M) {
    for (j in 1:N) {
      Cost[i,j] <- as.numeric((A[i,1] - B[j,1])^2 + (A[i,2] - B[j,2])^2) # distance function
    }
  }
  C <- matrix(0,M,N) # Initialize with zeros
  C[1,1] <- Cost[1,1] # Initialize top left cell
  for (i in 2:M) { # Initialize first column
    C[i,1] <- C[i-1,1] + Cost[i,1]
  }
  for (j in 2:N) { # Initialize first row
    C[1,j] <- C[1,j-1] + Cost[1,j]
  }
  # Main Loop
  for (i in 2:M) {
    for (j in 2:N) {
      C[i,j] <- min(c(C[i-1,j], C[i,j-1], C[i-1,j-1])) + Cost[i,j]
    }
  }
  return (C[M,N])
}

# identifying letters
lettera <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_A.csv")
lettere <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_E.csv")
letterm <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_M.csv")
lettero <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_O.csv")
letterunknown <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char4_.csv")
