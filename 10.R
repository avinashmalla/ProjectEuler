# https://projecteuler.net/problem=10
# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
# Find the sum of all the primes below two million.

library(gmp)

t <- proc.time()
sum_primes <- 0
for (i in 1:2000000) {
  if (isprime(i)) {
    sum_primes <- sum_primes + i
  }
}
print(paste("The sum of the above primes is ", sum_primes))
print(proc.time() - t)

## another method
t <- proc.time()
## Sieve of Eratosthenes for generating prime numbers 2:n
esieve <- function(n) {
  if (n == 1) {
    return(NULL)
  }
  if (n == 2) {
    return(n)
  }
  # Create a list l of consecutive integers {2,3,.....,n}.
  l <- 2:n
  # Start counter
  i <- 1
  # Select p as the first prime number in the list, p=2.
  p <- 2
  while (p^2 <= n) {
    # Remove all multiples of p from the l.
    l <- l[l == p | l %% p != 0]
    # set p equal to the next integer in l which has not been removed.
    i <- i + 1
    # Repeat steps 3 and 4 until p2 > n,
    # all the remaining numbers in the list are primes
    p <- l[i]
  }
  return(l)
}
answer <- sum(esieve(2 * 10^6))
print(answer)
print(proc.time() - t)
