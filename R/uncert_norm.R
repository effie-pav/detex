#'
#'@title Uncertainty propagation
#'@description The function applies a Taylor-based method to calculate frame uncertainty given the LST values of frame pixels and central pixel, and LST product-specified uncertainties for frame pixels and for central pixel time series 
#'@param data The dataframe containing time series of the normalization frame 
#'@param central The time series of the central pixel
#'@param un_central The uncertainties of the series of the central pixel
#'@param uncertainties The dataframe containing uncertainties of the series of the frame pixels
#'@param nr length of the time series (including slots without available measurements)
#'@examples
#
#'@author Effie Pavlidou
#'@export
#'


uncert_norm<-function(data, uncertainties, nr, central, un_central){
  u<-numeric(length=nr) #frame ucertainty
  un<-numeric(length=nr) #uncertainty of normalized series
  kernel<-numeric(length=nr) #time series of the average value of frame pixels
  
  for (i in 1:nr){
    #calculate existing values per frame, in LST and in LST uncertainties
  naf<-sum(is.na(data[i,]))
  nf<-sum(!is.na(data[i,]))
  na<-sum(is.na(uncertainties[i,]))
  n<-sum(!is.na(uncertainties[i,]))
    #calculate stats in the frame
  kernel[i]<-mean(as.numeric(data[i,], na.rm=TRUE))
  m<-mean(as.numeric(data[i,], na.rm=TRUE))
  v<-var(as.numeric(data[i,], na.rm=TRUE))
  #calculate correlated uncertainty component
  s<-sum((uncertainties[i,])^2)
  uc<-sqrt(s/n)
  #calculate sampling uncertainty component
  um<-(naf*v)/(naf+nf-1)
  #calculate total uncertainty in the frame
  u[i]<-sqrt((uc^2)+(um^2))
  }   
    #corrleation between central pixel and frame series
  rho<-as.numeric(cor(central, kernel, use="complete.obs", method = "pearson"))
    #uncertainty by Taylor method
  for (i in 1:nr) {
    if (is.na(kernel[i])){
      un[i]<-NA
    } else{
      tau_sq <- un_central[i,]^2/kernel[i]^2 + 
        u[i]^2*central[i,]^2/kernel[i]^4-2*rho*un_central[i,]*u[i]*central[i,]/kernel[i]^3
      un[i]<-sqrt(tau_sq)
    }
  }
  
  return(un)
  }