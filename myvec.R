library(tidyverse)
load("~/Desktop/myvec---Time-Series-/myvec.RData")

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
dtw(A, B)

# identifying letters
char_a <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_A.csv")
char_e <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_E.csv")
char_m <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_M.csv")
char_o <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char1_O.csv")
char_4 <- read_csv("/Users/nurhalizahassan/Desktop/myvec---Time-Series-/CSV_Files/char4_.csv")

# compare to letter a
distance_a <- dtw(char_a,char_4)
distance_a

# compare to letter e
distance_e <- dtw(char_e,char_4)
distance_e

# compare to letter m
distance_m <- dtw(char_m,char_4)
distance_m

# compare to letter o
distance_o <- dtw(char_o,char_4)
distance_o

min(c(distance_a, distance_e, distance_m, distance_o))

