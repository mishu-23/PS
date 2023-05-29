#D1

#(a)

gaseste = function(x, k) {
  n = length(x)

  for (i in 1:k) {
    random = sample(1:n, 1)
    val = x[random]
    
    if (sum(x == val) >= floor(n/2) + 1) {
      return(val)
    }
  }
  
  return("x nu are M-element")
}

#(b)
x = c(1, 2, 1, 4, 1, 1, 1, 1, 1, 10, 11, 1, 1, 1, 1, 16, 17, 18, 19, 20)
# 1/2^k <= 1/10^7  /:(-1)
# 2^k >= 10^7
#merge pentru k = 30
gaseste(x, 30)


#D2

ith_element = function(i, A) {
  n = length(A)
  
  z = sample(A, 1)
  A_less = A[A < z]
  A_greater = A[A > z]
  
  if (length(A_less) >= i) {
    return(ith_element(i, A_less))
  } else if (n > i + length(A_greater)) {
    return(z)
  } else {
    return(ith_element(i - n + length(A_greater), A_greater))
  }
}

A = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
i = 5
print(ith_element(i, A))


#D3

#(a)

approximate_median = function(S, a) {
  n = length(S)
  m = floor(a * log(n))
  S_prime = sample(S, m)
  sorted_S_prime = sort(S_prime)
  median_index = ceiling(m / 2)
  
  return(sorted_S_prime[median_index])
}

#(b)
# 1-2/n^2 >= 1-10^(-7)   /-1) /:(-1)
# 2/n^2 <= 10^(-7)  /:2  /sqrt
# n >= sqrt(2*10^7)
# n >= 4472.13 ...
x = runif(4473, 0, 100)
approximate_median(x, 500)
median(x)


