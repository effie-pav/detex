#'
#'@title Counting anomalies per period
#'@description The function counts number of anomalies in three user specified periods within the pixel's time series. Requires that a calender has been constructed as an ISOdate sequence.
#'@param predate the first date of interest in the format "yyyy-mm-dd hr:mn:sc GMT"
#'@param postdate the second date of interest in the format "yyyy-mm-dd hr:mn:sc GMT"
#'@param input the time series containing 1 for detected anomaly and NA for no detected anomaly
#'@examples
#
#
#'@author Effie Pavlidou
#'@export
#'

period_counts_simple<-function(predate, postdate, input){
  calender<-seq(ISOdate(2011,1,1, hour=0), ISOdate(2011, 12, 31, hour=23), by="hour")
  inmeans.d1<-c(0,0,0)
  inmeans.d1[1]<-mean(input[1:which(calender==predate)], na.rm=TRUE)
  inmeans.d1[2]<-mean(input[which(calender==predate):which(calender==postdate)], na.rm=TRUE)
  inmeans.d1[3]<-mean(input[which(calender==postdate):length(input)], na.rm=TRUE)

  return(inmeans.d1)
}
