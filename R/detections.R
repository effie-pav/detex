#'
#'@title Anomaly detection/ Change detection in time series
#'@description The script imports time series and uses different functions to perform anomaly and/or temporal change detection with different approaches. These include use of thresholds, counting flagged anomalies in rolling windows or discrete periods, calculating cumulative normalized values and using the Kolmoogorov-Smirnoff test to compare the distribution of values in different temporal windows. Plots are provided for comparisons.
#'@param
#'@param
#'@examples
#
#'@author Effie Pavlidou
#'@export
#'
detections<-function(input, lwin, k, ntot){
#example: input from single pixel

input<-as.numeric(input$x)
#set timestamp series
calender<-seq(ISOdate(2011,1,1, hour=0), ISOdate(2011, 12, 31, hour=23), by="hour")
input.x<-xts(x=input, order.by=calender)

#########################################################################
#use a threshold to delineate anomalies in the series
#this approach gives info on presence/absence of anomaly, not on intensity
flags<-flag(input, ntot)

#count anomalies in given periods
predate<-"2011-11-10 01:00:00 GMT"
postdate<-"2011-11-12 19:00:00 GMT"
counts<-period_counts_simple(predate, postdate, flags)

#count and plot nrs of anomalies in 7-day moving window
count7<-rollapply(flags, lwin, sum, fill = NA, na.rm=TRUE, partial = FALSE, align = "right")
plot.ts(count7)
##########################################################################

#calculate cumulative values in a 7-day moving window
#provides indication of anomaly intensity
sum7<-rollapply(input, lwin, sum, fill = NA, na.rm=TRUE, partial = FALSE, align = "right")
plot.ts(sum7)
###########################################################################

#use a two-sample KS test to see if the empirical distribution function
#in a temporal window is the same as in the previous window
#difference indicates possible change/anomaly
dstat<-ks(input, lwin)
dstatr<-ksr(input, lwin, k)
###########################################################################
}
