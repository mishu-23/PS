select_mean = function(filename) {
  x = scan(filename);
  m = mean(x);
  return(m);
}

select_mean("history.txt")

#I

alfa = 0.1
sample_mean = 20
n = 100
sigma = sqrt(9)
critical_z = qnorm(1 - alfa/2, 0, 1)
a = sample_mean - critical_z*sigma/sqrt(n)
b = sample_mean + critical_z*sigma/sqrt(n)
interval = c(a, b)
interval

#I.1

zconfidence_interval = function(n, sample_mean, alfa, sigma) {
  critical_z = qnorm(1 - alfa/2, 0, 1);
  a = sample_mean - critical_z*sigma/sqrt(n)
  b = sample_mean + critical_z*sigma/sqrt(n)
  interval = c(a, b)
  return(interval)
}

#II.2
zconfidence_interval(25, 67.53, 0.1, 10)

#II.3
zconfidence_interval(50, 5, 0.05, 0.5)

#II.6
zconfidence_interval_file = function(filename, alfa) {
  data = read.table(filename)
  n = length(data$V1)
  sigma = sd(data$V1)
  sample_mean = mean(data$V1) 
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return(interval)
}
zconfidence_interval_file("history.txt", 0.05)

#III

alfa = 0.05
sample_mean = 3.3
n = 60
s = 0.4
se = s/sqrt(n)
critical_t = qt(1 - alfa/2, n - 1)
a = sample_mean - critical_t*se
b = sample_mean + critical_t*se
interval = c(a, b)
interval

#III.1

t_conf_interval = function(n, sample_mean, s, alfa) {
  se = s/sqrt(n)
  critical_t = qt(1-alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  interval = c(a, b)
  return(interval)
}

#III.2

t_conf_interval(196, 44.65, sqrt(2.25), 0.01)

#III.3
#a

t_conf_interval(49, 12, 1.75, 0.01)
t_conf_interval(49, 12, 1.75, 0.05)

#b
t_conf_interval(49, 13.5, 1.25, 0.05)

#III.4

t_conf_interval_file = function(filename, alfa) {
  data = read.table(filename)
  n = length(data$V1)  
  sample_mean = mean(data$V1)  
  s = sd(data$V1)  
  se = s / sqrt(n) 
  critical_t = qt(1 - alfa/2, n - 1)  
  a = sample_mean - critical_t * se  
  b = sample_mean + critical_t * se 
  interval = c(a, b)
  return(interval)
}

#III.5

t_conf_interval_file("data.txt", 0.1)
t_conf_interval_file("data.txt", 0.05)  
t_conf_interval_file("data.txt", 0.01)  

#IV

alfa = 0.01
n = 100
succese = 63
p_prim = succese/n
p0 = 0.6
z_score = (p_prim - p0)/sqrt(p0(1 - p0)/n)
critical_z = qnorm(1 - alfa, 0, 1)
z_score
critical_z

#IV.1

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
    if(z_score <= critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
  if(tip_ip == "s") {
    critical_z = qnorm(1-alfa/2, 0, 1)
    cat("critical_z=", critical_z)
    if(abs(z_score) <= critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
}

test_proport(0.05, 150, 20, 0.1, "r")
