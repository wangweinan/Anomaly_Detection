ftx.func<-function(x, t, hx, ht){
  ## kernel estimator for conditional density
  ## Arguments:
  # x: a grid of points where densities are evaluated and observed
  # t: evaluated and observed covariate values corresponding to the chosen grid
  # hx: bandwidth for x
  # ht: bandwidth for t
  ## Values:
  # y: the densities evaluated
  n <- length(t)
  m <- length(x)
  y <- 0

  C <- sum(dnorm((t-t[m])/ht,0,1));
  numerator <- sum(dnorm((t-t[m])/ht,0,1)*dnorm((x-x[m])/hx,0,1)/hx);
  y <- numerator/C;
  return(y);
}
