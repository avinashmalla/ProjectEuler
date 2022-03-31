# https://projecteuler.net/problem=9

# A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
# For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
# There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.

t <- proc.time()
for (cC in (1:500^2)) {
  if(is.whole(sqrt(cC))){
    for (b in (1:(sqrt(cC)-1))) {
      for (a in 1:b) {
        aSQbSQ <- a^2 + b^2
        c <- sqrt(cC)
        sumABC <- a+b+c
        if(aSQbSQ == cC & sumABC == 1000){
          print(paste("a(",a,")","^2 + ", "b(",b,")","^2 = ", "c(",c,")","^2"))
          print(paste("a(",a,") + b (",b,") + c (",c,") = ", a+b+c))
          break
        }
      }
    }
  }
}
print(proc.time() - t)
