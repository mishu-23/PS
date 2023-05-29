#A1

#(a)

lambda = 2.5
p = 0.3
n = 10
k = 5

poisson = dpois(k:n, lambda)
geometric = dgeom(k:n, p)
b = dbinom(k:n, n, p)

plot(k:n, poisson, type = "n", ylim = c(0, max(poisson, geometric, b)),
     xlab = "x", ylab = "Prob.", main = "Grafic")
lines(k:n, poisson, type = "l", col = "blue")
lines(k:n, geometric, type = "l", col = "red")
lines(k:n, b, type = "l", col = "green")

#(b)

# P(X = impar)
p1 = 0
for(x in seq(1, n-k+1, 2)) {
  p1 = p1 + geometric[x]
}

# P(X >= 4)
p2 = 0
for(x in seq(4, n-k+1, 2)) {
  p2 = p2 + geometric[x]
}
# P(X <= 20)
p3 = 0
if(n-k+1>20) {
  for(x in seq(1, 20, 2)) {
    p3 = p3 + geometric[x]
  }
} else {
  for(x in seq(1, n-k+1, 2)) {
    p3 = p3 + geometric[x]
  }
}
cat("P(X = impar):", p1, "\n")
cat("P(X >= 4):", p2, "\n")
cat("P(X <= 20):", p3, "\n")

#(c)
k0 = 0
while (ppois(k0, lambda, lower.tail = FALSE) >= 10^(-7)) {
  k0 = k0 + 1
}
cat("P(Y >= k0) < 10^(-7):", k0, "\n")




#A2

#(a)

a = function(file) {
  data = read.csv(file)
  data_P = data$P
  data_S = data$S
  
  mediana_P = median(data_P)
  medie_P = mean(data_P)
  devstd_P = sd(data_P) 
  cvartile_P = quantile(data_P)
  
  mediana_S = median(data_S)
  medie_S = mean(data_S)
  devstd_S = sd(data_S) 
  cvartile_S = quantile(data_S)
  
  cat("P:\n")
  cat("Mediana:", mediana_P, "\n")
  cat("Media:", medie_P, "\n")
  cat("Deviatie Standard:", devstd_P, "\n")
  cat("Cvartile:", cvartile_P, "\n")

  cat("S:\n")
  cat("Mediana:", mediana_S, "\n")
  cat("Media:", medie_S, "\n")
  cat("Deviatie Standard:", devstd_S, "\n")
  cat("Cvartile:", cvartile_S, "\n")
}

a("note.csv")

#(b)

elim = function(file, samp_name) {
  data = read.csv(file)
  sample = data[[samp_name]]
  m = mean(sample)
  s = sd(sample)
  clean = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] >= (m - 2*s) & sample[i] <= (m + 2*s)) {
      j = j + 1
      clean[j] = sample[i]
    }
  return (clean)
}

elim("note.csv", "P")
elim("note.csv", "S")

#(c)

frecv = function(file) {
  data = read.csv(file)
  data_P = data$P
  data_S = data$S
  
  interval = seq(1, 10, by = 1)
  par(mfrow = c(1, 2)) #partitioneaza in 2 pentru a afisa ambele histograme
  hist(data_P, breaks = interval, col = "yellow")
  hist(data_S, breaks = interval, col = "red")
}

frecv("note.csv")