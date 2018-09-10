#'
#'@title Anomaly flagging
#'@description The function detects values that exceed a man+2sigma threshold and flags them as anomalies ("1"). Values not exceeding the threshold are replaced with NA
#'@param input The input time series 
#'@param nr The length of input time series (total number of observations)
#'@examples
#
#'@author Effie Pavlidou
#'@export
#'

flag<-function(input, nr){
 #constructs a series of nr observations that contains 1
  #where the input exceeds the set threshold, and zeros or NAs elsewhere
  flags<-c(as.numeric(1:nr))
  
  #calculating the anomaly threshold
  thr<-mean(input, na.rm=TRUE)+2*sd(input, na.rm=TRUE)
  #thresholding
    for (i in 1:nr){
    if (is.na(input[i]) | (input[i])<thr){
      flags[i]<-NA
    }
    else{
      flags[i]<-1
    }
  }
  return(flags)
}