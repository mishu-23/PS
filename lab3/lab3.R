  #I.

  density_exponential = function(lambda, n, a) {
    x = seq(0, a, n);
    y = dexp(x, lambda);
    plot(x, y, type = 'l');
  }
  
  #ex I.1. 
  
  #c) 
  
  plot_normal_density = function(mu, sigma2) {
    x = seq(mu - 4*sqrt(sigma2), mu + 4*sqrt(sigma2), length.out = 100)
    y = dnorm(x, mean = mu, sd = sqrt(sigma2))
    plot(x, y, type = "l", main = paste0("Normal Density: mu = ", mu, ", sigma^2 = ", sigma2))
  }
  
  plot_normal_density(0, 1)
  
  #II.
  
  LLN_Poisson = function(lambda, n) {
    return(mean(rpois(n, lambda)));
  }
  
  LLN_Gamma = function(alfa, lambda, n) {
    return(mean(rgamma(n, alfa, lambda)));
  }
  
  #II.1.
  
  #a)
  
  LLN_Exp =  function(lambda, n) {
    return (mean(rexp(m, lambda)));
  }
  
  #b)
  
  LLN_Binom = function(m, p, n) {
    return(mean(rbinom(n, m, p)));
  }
  
  #II.2.
  
  Students = function(r, n) {
    return(mean(rt(n, r)));
  }
  
  n_values = c(1000, 10000, 100000, 1000000)
  r_values = c(2, 3, 4, 5)
  
  for (n in n_values) {
    for (r in r_values) {
      result = Students(r, n)
      cat(paste0("n = ", n, ", r = ", r, ", result = ", result), "\n")
    }
  }
  
  #III.
  
  CLT_Poisson = function(lambda, n, N, z) {
    expectation = lambda;
    st_dev = lambda;
    upper_bound = z*st_dev/sqrt(n)+expectation;
    sum = 0;
    for(i in 1:N) {
      x_n = mean(rpois(n, lambda));
      if(x_n <= upper_bound) {
        sum = sum + 1;
      }
    }
    retrurn(sum/N);
  }
  
  #III.1.
  
  CLT_Exp = function(lambda, n, N, z) {
    expectation = 1/lambda;
    st_dev = 1/lambda;
    upper_bound = z*st_dev/sqrt(n)+expectation;
    sum = 0;
    for(i in 1:N) {
      x_n = mean(rexp(n, lambda));
      if(x_n <= upper_bound) {
        sum = sum + 1;
      }
    }
    return(sum/N);
  }
  
  CLT_Exp(2, 1000, 100000, 1)
  
  #III.2.
  
  CLT_Gamma = function(alpha, lambda, n, N, z) {
    expectation = alpha/lambda;
    st_dev = sqrt(alpha/lambda);
    upper_bound = z*st_dev/sqrt(n)+expectation;
    sum = 0;
    for(i in 1:N) {
      x_n = mean(rgamma(n, alpha, lambda));
      if(x_n <= upper_bound) {
        sum = sum + 1;
      }
    }
    return(sum/N);
  }
  
  N_values = c(5000, 10000, 20000)
  z_values = c(-1.5, 0, 1.5)
  n = 50
  alpha = 2
  lambda = 2
  
  for (N in N_values) {
    for (z in z_values) {
      result = CLT_Gamma(alpha, lambda, n, N, z);
      cat(paste0("N = ", N,", z = ", z,", result = ", result), "\n");
    }
  }
  
  #IV.2.
  
  binomial_probability = function(n, p, k) {
    expectation = n*p;
    variance = n*p*(1-p);
    standard_deviation = sqrt(variance);
    q = (k + 0.5 - expectation)/standard_deviation;
    return(1-pnorm(q));
  }
  
  binomial_probability(50, 0.3, 10)
  
  #IV.1.
  
  binomial_probability = function(n, p, k) {
    expectation = n*p;
    variance = n*p*(1-p);
    standard_deviation = sqrt(variance);
    q = (k + 0.5 - expectation)/standard_deviation;
    return(pnorm(q));
  }
  
  binomial_probability(50, 0.3, 10)