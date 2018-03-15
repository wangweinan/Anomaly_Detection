DenEst <- function(x.data,x.eval){
  den.Est <- density(x.data,from=min(x.data)-10,to=max(x.data)+10,n=1000);
  return(lin.itp(x.eval,den.Est$x,den.Est$y));
}
