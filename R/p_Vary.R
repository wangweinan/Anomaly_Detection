p_Vary <- function(x,tau,N){
# x is the observation vector
# lambda is the screening threshold
# N is the neighborhood size
  p <- rep(1,length(x));
  p_val <-2*pnorm(-abs(x));
for (i in 500:length(x)){
  neighbor <- max(1,(i-N)):i;
  #h <- density(neighbor)$bw;
  subset <- which(p_val[neighbor]>tau);
  p[i] <- min(1,DenEst(x[subset],x[i])/((1-tau)*DenEst(x[neighbor],x[i])));
}
  return(p);
}
