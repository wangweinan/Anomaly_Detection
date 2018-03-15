DenEst_Vary <- function(x.data,start,N){
  f_t <- rep(0,length(x.data));
  f_t[1:start] <- DenEst(x.data[1:start],x.data[1:start]);
  for (i in (start+1):length(x.data)){
    #np_bandwidth <- npcdensbw(min(1,(i-N)):i,x.data[min(1,(i-N)):i],bwmethod='normal-reference');
    #ht <- np_bandwidth$xbw;
    #hx <- np_bandwidth$ybw;
    #f_t[i] <- ftx.func(x.data[min(1,(i-N)):i],min(1,(i-N)):i,hx,ht);
    f_t[i] <-DenEst(x.data[(i-N-1):(i-1)],x.data[i]);
  }
  return(f_t);
}
