#C1

verif = function(x1, x2, x3, a) {
  x_squared = x2^2 / (1 + x3^2)
  return (x3 >= x_squared && x3 <= a)
}

estimate_volume = function(a, n_samples) {
  count = 0
  for (i in 1:n_samples) {
    x1 = runif(1, -sqrt(a), sqrt(a))
    x2 = runif(1, -sqrt(a), sqrt(a))
    x3 = runif(1, 0, a)
    if (verif(x1, x2, x3, a)) {
      count = count + 1
    }
  }
  volume = count / n_samples * (2 * sqrt(a))^3
  return (volume)
}

a_values = c(2, 4, 10)
n_samples = c(10000, 20000, 50000)

for (a in a_values) {
  for (n in n_samples) {
    estimated_volume = estimate_volume(a, n)
    exact_volume = pi * a^2 / 2
    relative_error = abs(estimated_volume - exact_volume) / exact_volume
    cat(paste0("a = ", a, ", n = ", n, ", volum estimat = ", estimated_volume, ", eroare relativa = ", relative_error), "\n")
  }
}

#C2

vertices = list()
vertices$x = c(0, (6 - 12)/(-3), max((6 - 12)/(-3), 0))
vertices$y = c((6 - 3*0)/3, 0, min((6 - 3*0)/3, (12 - 3*max(0, (6 - 12)/(-3)))/(-3)))

xmin = min(vertices$x)
xmax = max(vertices$x)
ymin = min(vertices$y)
ymax = max(vertices$y)

a = xmin
b = xmax
c = ymin
d = ymax

sample_size = 20000
points = data.frame(x = runif(sample_size, a, b), y = runif(sample_size, c, d))
inside_quadrilateral = points$x >= 0 & points$y >= 0 & 3*points$y <= points$x + 6 & points$y <= 12 - 3*points$x
area_estimate = (b - a) * (d - c) * sum(inside_quadrilateral) / sample_size

cat("Rectangular Region: [", a, ",", b, "] × [", c, ",", d, "]\n")
cat("Estimated Area:", area_estimate, "\n")

#C3

#(a)

MC_integration1 = function(N) {
  sum = 0
  for(i in 1:N) {
    u = runif(1, -1, 1)
    sum = sum + (u+1)/sqrt(4-u^2)
  }
  return((1-(-1))*sum/N)
}

MC_integr_average1 = function(k, N) {
  estimates = vector();
  for(i in 1:k) 
    estimates[i] = MC_integration1(N)
  return(mean(estimates));
}

exact_values = c(1.04)
estimated_values = MC_integr_average1(30, 1000)

# Calculating absolute and relative errors
absolute_errors = abs(estimated_values - exact_values)
relative_errors = absolute_errors / exact_values
print("Exact values of the integrals:")
print(exact_values)
print("Estimated values of the integrals:")
print(estimated_values)
print("Corresponding absolute errors:")
print(absolute_errors)
print("Corresponding relative errors:")
print(relative_errors)

#(b)

MC_improved_integration2 = function(N) {
  sum = 0
  for(i in 1:N) {
    u = runif(1, 0, 1)
    sum = sum + (1/(u^2+4))
  }
  return((1-(-1))*sum/N)
}
MC_integr_average2 = function(k, N) {
  estimates = vector();
  for(i in 1:k) 
    estimates[i] = MC_improved_integration2(N)
  return(mean(estimates));
}

exact_values = c(0.78)
estimated_values = MC_integr_average2(30, 1000)

# Calculating absolute and relative errors
absolute_errors = abs(estimated_values - exact_values)
relative_errors = absolute_errors / exact_values
print("Exact values of the integrals:")
print(exact_values)
print("Estimated values of the integrals:")
print(estimated_values)
print("Corresponding absolute errors:")
print(absolute_errors)
print("Corresponding relative errors:")
print(relative_errors)

#(c)

MC_improved_integration3 = function(N) {
  sum = 0
  for (i in 1:N) {
    u = runif(1, -1, 0)
    sum = sum + u * exp(u)
  }
  return(sum / N)
}

MC_integr_average3 = function(k, N) {
  estimates = vector();
  for(i in 1:k) 
    estimates[i] = MC_improved_integration3(N)
  return(mean(estimates));
}

exact_values = c(-1)
estimated_values = MC_integr_average3(30, 1000)

# Calculating absolute and relative errors
absolute_errors = abs(estimated_values - exact_values)
relative_errors = absolute_errors / exact_values
print("Exact values of the integrals:")
print(exact_values)
print("Estimated values of the integrals:")
print(estimated_values)
print("Corresponding absolute errors:")
print(absolute_errors)
print("Corresponding relative errors:")
print(relative_errors)
