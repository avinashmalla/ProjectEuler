# https://projecteuler.net/problem=5
# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

library(stringr)

num <- 1
finalNum <- 0
flag <- 0
cond <- str_c("num %% ", 2:20, " == 0", collapse = " & ")
while (flag == 0) {
  if (eval(parse(text = cond))) {
    finalNum <- num
    flag <- 1
  }
  num <- num + 1
}
print(finalNum)

## VERY SLOW. COME UP WITH ANOTHER WAY
