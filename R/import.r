#'
#'@title import
#'@description function to input multiple time series. The series should be stored as 'f*.txt' (f1.txt, f2.txt etc). The function returns a dataframe where each column corresponds to one pixel's time series 
#'@param n the number of series to be imported 
#'@examples
#'
#'@author Effie Pavlidou
#'@export
#'

import<-function(n){
files<-lapply(1:n, function(x){paste0("f", x, ".txt")})
names<-lapply(1:n, function(x){paste0("f", x)})
data<-as.data.frame(sapply(files, function(x){scan(x)}))
colnames(data)<-names
return(data)
}
