#'
#'@title Spatiotemporal normalization
#'@description The function normalizes the central pixel with the average of a frame of neighbouring pixels to suppress commonalities in the signal.The process is repeated in every timeslot to produce a time series of normalized values.
#'@param nr the length of the time series (nr of observations)
#'@param central the time series of the central pixel 
#'@param data the dataframe containing the time series of the pixels belonging to the normalization frame 
#'@examples
#
#
#'@author Effie Pavlidou
#'@export
#'

normalize<-function(nr, central, data){
  normalized<-numeric(length=nr)
  kernel<-numeric(length=nr) #time series of the average value of frame pixels
  
  for (i in 1:nr){
    kernel[i]<-mean(as.numeric(data[i,], na.rm=TRUE))
    if (is.na(kernel[i])){
      normalized[i]<-NA
    } else{
      normalized[i]<-central[i,]/kernel[i]
    }
  }
  return(normalized)
}