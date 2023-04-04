#9.
Geometric = function(p, n) {
  prob = dgeom(1:n, p);
  barplot(prob);
  prob;
}
Geometric(0.5, 10);

#10.
Poisson = function(lambda, n) {
  prob = dpois(1:n, lambda);
  barplot(prob);
  prob;
}
Poisson(0.5, 10);