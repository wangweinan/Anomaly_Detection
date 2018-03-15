LOND <- function(m,mu,p,t,sigma=1,alpha=0.05,iter){
  #Initialize Vectors
  decision <- x.Obs <- x.Real <- rep(0,m);
  #Ground Truths, p here is a vector
  x.Real <- p %>% map_dbl(~sample(c(0,1),1,prob=c(1-.x,.x)));
  theta <- x.Real*mu;
  #Observations for x
  x.Obs <- theta %>% map_dbl(~rnorm(1,.x,sigma));
  x.pval <- 2*pnorm(-abs(x.Obs));
  #Intializing constant
	constant <- 1:m %>% map_dbl(~1/(.*log(.+1)^2)) %>% sum;
	beta <- 1:m %>% map_dbl(~alpha/(constant*.*log(.+1)^2));
  #LOND procedure
	rej.num <- 0;
	for(i in 1:m){
		if(x.pval[i]<=beta[i]*(rej.num+1)){
			decision[i] <- 1;
			rej.num <- rej.num+1;
		}
	}
	#Result
	res <- t %>% map(~FDP_MDP(x.Real,decision,.x)) %>% unlist %>% split(.,names(.));
	return(list(mu=mu,iter=iter,FDP=res$FDP,t=res$t,MDP=res$MDP));
}





