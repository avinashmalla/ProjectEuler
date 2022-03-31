# https://projecteuler.net/problem=7

# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
# What is the 10 001st prime number?

library(gmp)

num <- 0
countPrime <- 1
while (countPrime <= 10001) {
  num <- num + 1
  if(isprime(num)){
    countPrime <- countPrime + 1
  }
}

print(paste("The 10001st prime number is ", num))
