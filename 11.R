# https://projecteuler.net/problem=11
# In the 20x20 grid below, four numbers along a diagonal line have been marked in red.

# 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
# 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
# 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
# 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
# 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
# 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
# 32 98 81 28 64 23 67 10 (26) 38 40 67 59 54 70 66 18 38 64 70
# 67 26 20 68 02 62 12 20 95 (63) 94 39 63 08 40 91 66 49 94 21
# 24 55 58 05 66 73 99 26 97 17 (78) 78 96 83 14 88 34 89 63 72
# 21 36 23 09 75 00 76 44 20 45 35 (14) 00 61 33 97 34 31 33 95
# 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
# 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
# 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
# 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
# 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
# 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
# 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
# 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
# 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
# 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

# The product of these numbers is 26 x 63 x 78 x 14 = 1788696.
# What is the greatest product of four adjacent numbers in the same
# direction (up, down, left, right, or diagonally) in the 20x20 grid?


library(rvest)
digitMain <- read_html("https://projecteuler.net/problem=11") %>%
  html_nodes("p") %>%
  html_text()

digits <- digitMain
# digits <- gsub("[^0-9\\.]", " ", digits[2])
digits <- as.numeric(unlist(strsplit(gsub("[^0-9\\.]", " ", digits[2]), " ")))

digits <- digits[!is.na(digits)]
digitMatrix <- matrix(digits, nrow = 20, byrow = T)

testMatrix <- matrix(nrow = 4, ncol = 4)

prodRows <- 0 # stores the products of 4 consecutive numbers in all rows
prodCols <- 0 # stores the products of 4 consecutive numbers in all columns
prodDiagUp <- 0 # stores the products of 4 consecutive numbers diagonally up
prodDiagDown <- 0 # stores the products of 4 consecutive numbers diagonally down

for (j in 1:17) {
  for (i in 1:17) {
    testMatrix <- digitMatrix[i:(i + 3), j:(j + 3)]
    rotate <- function(x) t(apply(x, 2, rev))
    rotatedMatrix <- rotate(testMatrix)

    for (x in 1:4) {
      if ((prod(testMatrix[x, ])) > prodRows) {
        prodRows <- prod(testMatrix[x, ])
        maxRow <- testMatrix[x, ] # this stores the row (4 consec. nums) that produces max product
        maxRowI <- i
        maxRowJ <- j
      }
      if (prod(testMatrix[, x]) > prodCols) {
        prodCols <- prod(testMatrix[, x])
        maxCol <- testMatrix[, x] # stores the col (4 consec. nums) that produces max product
        maxColI <- i
        maxColJ <- j
      }
      if (prod(diag(testMatrix)) > prodDiagDown) {
        prodDiagDown <- prod(diag(testMatrix))
        maxDiagDown <- diag(testMatrix)
        maxDiagDownI <- i
        maxDiagDownJ <- j
      }
      if (prod(diag(rotatedMatrix)) > prodDiagUp) {
        prodDiagUp <- prod(diag(rotatedMatrix))
        maxDiagUp <- diag(rotatedMatrix)
        maxDiagUpI <- i
        maxDiagUpJ <- j
      }
    }
  }
}
paste(c("Largest 4 consecutive numbers in rows: ", maxRow), collapse = " ")
print(paste("The product is : ", prodRows))
# digitMatrix[maxRowI:(maxRowI + 3), maxRowJ:(maxRowJ + 3)]
# print(paste("Key => Row:", maxRowI, "Col:", maxRowJ))

paste(c("Largest 4 consecutive numbers in columns: ", maxCol), collapse = " ")
print(paste("The product is : ", prodCols))
# digitMatrix[maxColI:(maxColI + 3), maxColJ:(maxColJ + 3)]
# print(paste("Key => Row:", maxColI, "Col:", maxColJ))

paste(c("Largest 4 consecutive numbers diagonally down: ", maxDiagDown), collapse = " ")
print(paste("The product is : ", prodDiagDown))
digitMatrix[maxDiagDownI:(maxDiagDownI + 3), maxDiagDownJ:(maxDiagDownJ + 3)]
# print(paste("Key => Row:", maxDiagDownI, "Col:", maxDiagDownJ))

paste(c("Largest 4 consecutive numbers diagonally up: ", maxDiagUp), collapse = " ")
print(paste("The product is : ", prodDiagUp))
digitMatrix[maxDiagUpI:(maxDiagUpI + 3), maxDiagUpJ:(maxDiagUpJ + 3)]
# print(paste("Key => Row:", maxDiagUpI, "Col:", maxDiagUpJ))

cat("\n", "GREATEST PRODUCT OF ADJACENT NUMBERS IS: ", max(prodRows, prodCols, prodDiagDown, prodDiagUp))
