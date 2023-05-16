#I.

disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  return (4*N_C/N);
}

disc_area(100000)

#I.1.

sphere_Vol = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z=runif(1, -1, 1);
    if(x*x + y*y+z*z <= 1)
      N_C = N_C + 1;
  }
  return(8*N_C/N); 
}
abs_err=abs(sphere_Vol(10000)-4*pi/3);
rel_err=abs_err/(4*pi/3)*100;
print(abs_err);
print(rel_err);

#I.2.

parabola = function(x) {
  return(-2 * x^2 + 5 * x - 2);
}

estimate_area = function(num_points) {
  count = 0;
  
  for (i in 1:num_points) {
    x = runif(1, 0, 2);
    y = runif(1, 0, 2);
    
    if (y <= parabola(x)) {
      count = count + 1;
    }
  }
  
  area_estimate = (count / num_points) * 4;
  
  return(area_estimate);
}

exact_area = function() {
  a = 0;
  b = 2;
  n = 10000;
  dx = (b - a) / n;
  
  area = 0;
  
  for (i in 1:n) {
    x = a + i * dx;
    area = area + parabola(x) * dx;
  }
  
  return(area);
}

relative_error = function(estimated_area, exact_area) {
  error = abs(estimated_area - exact_area);
  relative_error = error / exact_area * 100;
  
  return(relative_error);
}

estimated_area = estimate_area(10000);

exact = exact_area();

error = relative_error(estimated_area, exact);

print(paste("Aria estimată:", estimated_area));
print(paste("Aria exactă:", exact));
print(paste("Eroarea relativă:", error, "%"));

#II.2
