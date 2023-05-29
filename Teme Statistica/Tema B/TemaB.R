#B1

LLN_Geometric = function(n, p) {
  return(mean(rgeom(n, p)));
}

n_values = c(5000, 10000, 100000, 500000)
p_values = c(0.2, 0.6, 0.6, 0.8)

for (n in n_values) {
  for (p in p_values) {
    cat("Pentru n =", n, "si p =", p, ":\n")
    result = LLN_Geometric(n, p)
    cat("Valoarea estimata a mediei:", result, "\n")
    cat("Valoarea exacta a mediei:", 1/p, "\n\n")
  }
}

#B2

CLT_Students = function(r, n, N, z) {
  expectation = 0;
  st_dev = sqrt(r/(r-2));
  upper_bound = z*st_dev/sqrt(n)+expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rt(n, r));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  return(sum/N);
}

N_values = c(5000, 10000, 20000)
z_values = c(-1.5, 0, 1.5)
n = 50
r = 3

for (N in N_values) {
  for (z in z_values) {
    result = CLT_Students(r, n, N, z);
    eroare_relativa = abs(pnorm(z) - result) / pnorm(z)
    cat(paste0("N = ", N,", z = ", z,", eroare relativa = ", eroare_relativa), "\n");
  }
}

#B3

moivre = function(n, p, h, k) {
  expectation = n*p;
  variance = n*p*(1-p);
  standard_deviation = sqrt(variance);
  prob = pnorm(k + 0.5, expectation, standard_deviation) - pnorm(h + 0.5, expectation, standard_deviation)
  return(prob);
}

moivre(100, 0.3, 20, 40)