Oracle_SunCai <- function(m,mu,p,t,sigma=1,alpha=0.05,iter){
  #Initialize Vectors
  decision <- x.Obs <- x.Real <- rep(0,m);
  #Ground Truths, p here is a vector
  x.Real <- p %>% map_dbl(~sample(c(0,1),1,prob=c(1-.x,.x)));
  theta <- x.Real*mu;
  #Observations for x
  x.Obs <- theta %>% map_dbl(~rnorm(1,.x,sigma));
  #Proportion estimation
  p.Est <- epsest.func(x.Obs,0,1);
  #Density estimation
  den.Est <- DenEst(x.Obs);
  #Lfdr estimation
  x.Lfdr <- (1-p.Est)*dnorm(x.Obs,0,1)/den.Est;
  #Moving average calculation <-
  x.Lfdr.sorted <- sort(x.Lfdr[1:m],index.return=TRUE);
  x.Lfdr.mv <- cumsum(x.Lfdr.sorted$x)/1:m;
  #Optimal threshold
  gamma <- x.Lfdr.sorted$x[max(which(x.Lfdr.mv<=alpha))]
  #Decision
  decision[x.Lfdr<=gamma] <- 1;
  #Result
  res <- t %>% map(~FDP_MDP(x.Real,decision,.x)) %>% unlist %>% split(.,names(.))
  return(list(mu=mu,iter=iter,FDP=res$FDP,MDP=res$MDP,gamma=gamma));
}






