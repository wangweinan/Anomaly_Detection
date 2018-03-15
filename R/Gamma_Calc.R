Gamma_Calc <- function(x.Lfdr,t,alpha){
  #Moving average calculation
  x.Lfdr.sorted <- sort(x.Lfdr[1:t],index.return=TRUE);
  x.Lfdr.mv <- cumsum(x.Lfdr.sorted$x)/1:t;
  #Optimal threshold
  if(sum(x.Lfdr.mv<=alpha)!=0){
    gamma <- x.Lfdr.sorted$x[max(which(x.Lfdr.mv<=alpha))];
  } else{
    gamma <- 1;
  }
  return(gamma);
}
