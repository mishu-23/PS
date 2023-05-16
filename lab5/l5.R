x = sample(300,1)
x = sample(200:250,5)
x = c(2.1, 3.2, 2.3, 2.5, 3.1, 2.9, 2.6, 2.2)
sample(x, 5, replace=T)
runif(10, 2, 4.5)

matrix_product = function(A, B, C) {
  n = nrow(A);
  r = matrix( , nrow = n, ncol = 1);
  x = matrix( , nrow = n, ncol = 1);
  y = matrix( , nrow = n, ncol = 1);
  r = sample(0:1, n, replace = TRUE);
  for(i in 1:n) {# x = Br
    x[i] = 0;
    for(j in 1:nrow(B))
      x[i] = (x[i]+ B[i,j]*r[j])%%2;
  }
  for(i in 1:nrow(B)) {# y = Ax = ABr
    y[i] = 0;
    for(j in 1:n)
      y[i] = (y[i]+ A[i,j]*x[j])%%2;
  }
  for(i in 1:n) {# x = Cr
    x[i] = 0;
    for(j in 1:n)
      x[i] = (x[i]+ C[i,j]*r[j])%%2;
  }
  for(i in 1:n) {# verify if ABr==Cr
    if(y[i] !=x[i])
      return(FALSE);
  }
  return(TRUE);
}

x = c(0, 0, 1, 0, 1, 0, 1, 1, 0)
A = matrix(x, 3, 3)
y = c(1, 1, 0, 1, 0, 1, 1, 1, 0)
B = matrix(y, 3, 3)
D = A%*%B
Z = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
M = matrix(Z, 3, 3)
C = D-M
matrix_product(A, B, C)






simulare = function(mat) {
  vals = matrice_X[, 1]
  probs = matrice_X[, 2]
  
  if (length(vals) != length(probs)) {
    stop("numar de valori diferit de numar de probabilitati")
  }
  
  n = length(vals)
  sumProbs = sum(probs)
  
  if (sumProbs != 1) {
    stop("suma prob e dif de 1")
  }
  
  u = runif(1)
  cumProbs = cumsum(probs)
  
  for (i in 1:n) {
    if (u <= cumProbs[i]) {
      return(vals[i])
    }
  }
}

mat = matrix(c(1, 0.1, 2, 0.3, 3, 0.3, 4, 0.1, 5, 0.2), nrow = 5, byrow = TRUE)
print(simulare(mat))