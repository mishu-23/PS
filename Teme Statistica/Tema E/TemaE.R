#E1

t_conf_interval = function(n, sample_mean, s, alfa){
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  interval = c(a, b)
  return(interval)
}

t_conf_interval(10, 138, 11, 0.1)
t_conf_interval(10, 138, 11, 0.05)
t_conf_interval(10, 138, 11, 0.01)

#E2

t_conf_interval(256, 18, sqrt(1.44), 0.05)

#E3

test_proport = function(alfa, n, succese, p0, tip_ip) {
  p_prim = succese/n;
  z_score = (p_prim - p0)/sqrt(p0*(1-p0)/n)
  if(tip_ip == "r"){
    critical_z = qnorm(1-alfa, 0, 1)
    cat("critical_z=", critical_z)
    if(z_score <= critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
  if(tip_ip == "l") {
    critical_z = qnorm(alfa, 0, 1)
    cat("critical_z=", critical_z)
    if(z_score >= critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
  if(tip_ip == "s") {
    critical_z = qnorm(1-alfa/2, 0, 1)
    cat("critical_z=", critical_z)
    if(abs(z_score) <= abs(critical_z))
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
}

test_proport(0.01, 153, 17, 0.12, "r")
test_proport(0.05, 153, 17, 0.12, "r")
