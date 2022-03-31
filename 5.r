# https://projecteuler.net/problem=5
# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

# Essentially, the question says 2520 is the LCM for 1:10 [i.e. 1,2,3,4,5,6,7,8,9,10] is 2520.
# What is is the LCM for 1:20?

library(stringr)

hcf = function (x, y){
  ifelse(x == 0, y, hcf(y %% x, x))
}
lcm = function (x, y){
  x * y / hcf(x, y)
}

print(Reduce(lcm, 1:20))
