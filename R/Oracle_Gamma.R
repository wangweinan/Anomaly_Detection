Oracle_Gamma <- function(m,mu,p,t,sigma=1,alpha=0.05,iter){
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
  #Optimal threshold
  gamma <- 1:m %>% map_dbl(~Gamma_Calc(x.Lfdr,.,alpha));
  #Initialize active set
  rej.num <- active.Lfdr.mva <- 0;
  #Online FDR Procedure: Update threshold at each step
  for (i in 1:m){
    if(x.Lfdr[i]<=gamma[i]){
      if ((active.Lfdr.mva*rej.num+x.Lfdr[i])/(rej.num+1)<=alpha){
        decision[i] <-1;
        active.Lfdr.mva <- (active.Lfdr.mva*rej.num+x.Lfdr[i])/(rej.num+1);
        rej.num <- rej.num+1;
      }
    }
  }
  #Result
  res <- t %>% map(~FDP_MDP(x.Real,decision,.x)) %>% unlist %>% split(.,names(.))
  return(list(mu=mu,iter=iter,FDP=res$FDP,MDP=res$MDP,t=res$t,gamma=gamma));
}






