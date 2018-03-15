# calculate FDP and MDP given the decision and the truth
FDP_MDP <- function(x.Real,dec,t){
  if (sum(dec[1:t])!=0){
    FDP <- sum((1-x.Real[1:t])*dec[1:t])/(sum(dec[1:t]));
  } else {
    FDP <- 0;
  }
  if (sum(x.Real[1:t])!=0){
    MDP <- sum(x.Real[1:t]*(1-dec[1:t]))/sum(x.Real[1:t]);
  } else {
    MDP <- 0;
  }
  return(list(FDP=FDP,MDP=MDP,t=t));
}
