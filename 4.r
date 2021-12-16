# https://projecteuler.net/problem=4
# A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 <U+00D7> 99.
# Find the largest palindrome made from the product of two 3-digit numbers.

library(stringr)
library(stringi)

checkPalin <- function(checkTerm) {
  flag <- 0
  checkTerm <- as.character(checkTerm)
  numDigits <- str_length(checkTerm)
  if ((numDigits %% 2) == 0) {
    # print(paste(checkTerm,"has Even number of characters"))
    left <- str_sub(checkTerm, 1, (numDigits / 2))
    right <- str_sub(checkTerm, ((numDigits / 2) + 1), (numDigits))
    if (identical(left, stri_reverse(right))) {
      flag <- 1
    } else {
      flag <- 0
    }
  } else {
    # print(paste(checkTerm,"has Odd number of characters"))
    left <- str_sub(checkTerm, 1, (floor(numDigits / 2)))
    right <- str_sub(checkTerm, ((ceiling(numDigits / 2)) + 1), (numDigits))
    if (identical(left, stri_reverse(right))) {
      flag <- 1
    } else {
      flag <- 0
    }
  }
  return(flag)
}

first <- 0
second <- 0
prevProd <- 0
for (i in 999:100) {
  for (j in 999:100) {
    product <- i * j
    if (checkPalin(product) & prevProd < product) {
      first <- i
      second <- j
      prevProd <- first * second
    }
  }
}
print(paste(first, " x ", second, "= ", first * second))
