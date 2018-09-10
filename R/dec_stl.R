#'
#'@title Time series decomposition
#'@description The function performs Seasonal-Trend Decomposition by Loess, based on package stlplus (required). It plots and returns the component(s) defined by the user.
#'@param n One or all of the components of the decomposition, or their combinations. Should be named "trend", "seasonal", "remainder", "trendseas", "trendrem", or "seasrem"
#'@param series The time series to be decomposed, as a single column dataframe
#'@examples
#
#'@author Effie Pavlidou
#'@export
#'

dec_stl<-function(series, n){
input<-as.numeric(series[,1])
input.ts<-ts(input)
input.stl<-stlplus(input.ts, t=NULL, 24, s.window=71, s.degree=0, t.window=37, t.degree=0, fc.window=8760, fc.degree=2, l.window=25, l.degree=0, inner=1, outer=5)
p<-plot(input.stl)

#daily<-movavg(input.ts, 24, type="s")

component<-  switch(n,
         trend = ts(fc(input.stl)),
         seasonal = ts(seasonal(input.stl)),
         remainder = ts(remainder(input.stl)),
         trendseas = ts(fc(input.stl)+seasonal(input.stl)),
         trendrem = ts(fc(input.stl)+remainder(input.stl)),
         seasrem = ts(seasonal(input.stl)+remainder(input.stl)))

plot.ts(component, ylab=n, xlab="Time")
return(component)

}


