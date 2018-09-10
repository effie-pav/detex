#'
#'@title Kolmogorov-Smirnoff two-sample test in discrete temporal windows
#'@description The function is based on the ks.test function of the stats package. It tests if the subseries in a temporal window y and in its previous window x come from the same empirical distribution. It calculates the D-statistic at level 0.05 as threshold, it returns D-statistic time series for each window and plots D-statistics with the threshold and p-values. For calculation of the threshold, c(a)=1.36 for level a=0.05, c=1.95 for a=0.001. With D-stat>D95% and p-value small, the samples come from different distributions.
#'@param input the input values of the time series, not as a ts object
#'@param lwin the length of the temporal window 
#'@examples
#
#
#'@author Effie Pavlidou
#'@export
#'


ks<-function(input, lwin){
  windows<-matrix(input, nrow=lwin)
  sums<-colSums(!is.na(windows))
  dstat<-numeric(length=ncol(windows))
  d0<-numeric(length=ncol(windows))
  pval<-numeric(length=ncol(windows))
  dstat[1]<-NA
  d0[1]<-NA
  pval[1]<-NA


  
  for (i in 2:ncol(windows)){
    d0[i]<-1.36*sqrt((sums[i]+sums[i-1])/(sums[i]*sums[i-1]))
    result<-ks.test(windows[,i], windows[,i-1], alternative="two.sided", exact=NULL)
    dstat[i]<-result[1]
    pval[i]<-result[2]
  }
  
  plot.ts(dstat, ylab=" ", xlab="Time", ylim=c(0,1))
  par(new=T)
  plot.ts(d0, col="red", ylab=" ", xlab="", ylim=c(0,1))
  par(new=T)
  plot.ts(pval, col="lightskyblue", ylab=" ", xlab="Time", ylim=c(0,1))
  legend(1, 1, legend=c("D-statistic", "D95%", "p-value"), col=c("black", "red", "lightskyblue"), box.lty = 0, lty=c(1,1,1), cex=0.8)
  
  return(dstat)
}
