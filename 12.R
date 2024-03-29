# https://projecteuler.net/problem=12
# The sequence of triangle numbers is generated by adding the natural numbers. 
# So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. 
# The first ten terms would be:
# 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, …
# 
# Let us list the factors of the first seven triangle numbers:
# 1: 1 
# 3: 1,3 
# 6: 1,2,3,6 
# 10: 1,2,5,10 
# 15: 1,3,5,15 
# 21: 1,3,7,21 
# 28: 1,2,4,7,14,28 
# We can see that 28 is the first triangle number to have over five divisors.
# What is the value of the first triangle number to have over five hundred divisors?

#500 was too much processing time. so i switched to 100
t <- proc.time()
nth <- 1
numDivisors <- 0
numDivisorsLimit <- 100
while(numDivisors <= numDivisorsLimit){
  ttriangle <- function(n){
    vect <- 0
    vect[1] <- 0
    for (i in 1:n) {
      vect[i] <- sum(1:i)
    }
    return(vect)
  }
  
  triangularVector <- triangle(nth)
  (triangularNum <- triangularVector[nth])
  
  extractDivisors <- function(num){
    div <- 0
    j <- 1
    for (i in 1:num) {
      if(num %% i == 0){
        div[j] <- i
        j <- j + 1
      }
    }
    return(div)
  }
  nth <- nth + 1
  numDivisors <- length(extractDivisors(triangularNum))
  if(numDivisors >= numDivisorsLimit){
    print(triangularNum)
  }
}
print(proc.time() - t)

euler_12 <- function(x) {
  max_factors <- 0
  val <- 1
  
  while (max_factors < x) {
    target_num <- triangle(val)[val]
    
    factors <- unique(unlist(find_factors(target_num)))
    if (length(factors) > max_factors) {
      max_factors <- length(factors)
    }
    val <- val + 1
  }
  
  target_num
}

seq(1:7)

triangle <- function(nth) {
  vals <- seq(from = 1, to = nth)
  cumsum(vals)
}


find_factors <- function(x) {
  factors <- list()
  
  for (i in 1:sqrt(x)) {
    if (x %% i == 0) {
      factors[[length(factors) + 1]] <- c(i, x / i)
    }
  }
  
  factors
}

find_factors(10)

euler_12(500)

