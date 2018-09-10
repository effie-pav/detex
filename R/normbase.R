#'
#'@title Complete spatiotemporal normalization
#'@description The script imports time series, calculates normalized series, estimates input uncertainty propagation in normalized values and plots normalized series with their uncertainty.
#'@param
#'@param
#'@examples
#
#
#'@author Effie Pavlidou
#'@export
#'


#import data (adjust names and formats depending on available data formats, naming and location)
#central<-read.table("central.txt")
#un_central<-read.table("un_central.txt")
#instead of reading an existing dataframe, possible to call function import: data<-import(n)
#data<-read.table("data.txt")
#uncertainties<-read.table("uncertainties.txt")
normbase<-function(central, data, uncertainties, un_central){
#normalize central pixel with the average of the frame
nr<-as.numeric(length(ts(central)))
normalized<-normalize(nr, central, data)

#uncertainty of normalized series
un<-uncert_norm(data, uncertainties, nr, central, un_central)
#upper and lower bounds
u<-normalized+un
l<-normalized-un

#plot detail. Just an example for the given dataset, adjust to dataset at-hand
display.c<-normalized[4008:4217]
display.u<-u[4008:4217]
display.l<-l[4008:4217]
y<-as.numeric(1:210)
y2<-c(y,210,rev(y),1)
plot(y,display.u,type="l",bty="L",xlab="time",ylab="Normalized values", ylim=c(0.995, 1.008), col="white", axes=FALSE)
par(new=T)
plot(y,display.l,type="l",bty="L",xlab="time",ylab="Normalized values", ylim=c(0.995, 1.008), col="white", axes=FALSE)
polygon(y2,c(display.u, display.u[210], rev(display.l), display.l[1]),col="skyblue", border="skyblue")
par(new=T)
plot.ts(display.c, lwd=2, col="black", ylim=c(0.995, 1.008), ylab="", xlab="", axes=FALSE)
axis(2)
axis(1, at=c(1, 72, 144, 192), labels=c("June 16", "June 19", "June 21", "June 23"))
box()
}
