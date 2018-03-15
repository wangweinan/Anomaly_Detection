Oracle_Gamma_Unknown <- function(t,k){
	m <- 5000;
	decision <- rep(0,m);
	mu <- 3;
	p <- 0.05;
	sigma <- 1;

	alpha <- 0.05;

	x.Obs <- rep(0,m);
	x.Real <- rep(0,m);

	x.Real <- sample(c(rep(0,m*(1-p)),rep(1,m*p)),replace=FALSE,prob=NULL);
	theta <- x.Real*mu;

	for (i in 1:m){
		x.Obs[i] <- rnorm(1,theta[i],1);
	}

	# gamma <- rep(1,m);
	# gamma.pot <-seq(10,0,length.out=10000);
	# x.Lfdr.test <- (1-p)*dnorm(gamma.pot,0,1)/((1-p)*dnorm(gamma.pot,0,1)+p*dnorm(gamma.pot,mu,1));
	# x.Lfdr.test.sorted <- sort(x.Lfdr.test,decreasing=FALSE,index.return=TRUE);
	# x.Lfdr.test.cumsum <- cumsum(x.Lfdr.test.sorted$x);

	# for (i in 1:length(gamma.pot)){
	# 	if(x.Lfdr.test.cumsum[i]/i <= alpha & x.Lfdr.test.cumsum[i+1]/(i+1) > alpha){
	# 		gamma <- x.Lfdr.test.sorted$x[i];
	# 		break;
	# 	}
	# }


	x.Lfdr <- (1-p)*dnorm(x.Obs,0,1)/((1-p)*dnorm(x.Obs,0,1)+p*dnorm(x.Obs,mu,1));
	buffer.Lfdr.rej <- NULL;
	rej.num <- 0;
	buffer.Lfdr.mva <- 0;
	for (i in 1:m){
		# if(x.Lfdr[i]<=gamma[i]){
			if ((buffer.Lfdr.mva*rej.num+x.Lfdr[i])/(rej.num+1)<=alpha){
				decision[i] <-1;
				buffer.Lfdr.rej <- append(buffer.Lfdr.rej,x.Lfdr[i]);
				buffer.Lfdr.mva <- (buffer.Lfdr.mva*rej.num+x.Lfdr[i])/(rej.num+1);
				rej.num <- rej.num+1;  
				# gamma[i+1] <- max(buffer.Lfdr.rej);
			}
		# }
	}


		
	if (sum(decision[1:tau[t]])!=0){
  		FDP <- sum((1-x.Real[1:tau[t]])*decision[1:tau[t]])/(sum(decision[1:tau[t]]));
	}
	if (sum(decision[1:tau[t]])==0){
  		FDP <- 0;
	}
  	MDP <- sum(x.Real[1:tau[t]]*(1-decision[1:tau[t]]))/sum(x.Real[1:tau[t]]);

  	return(c(t,k,FDP,MDP));
}





