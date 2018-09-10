\name{decompositions}
\alias{decompositions}

\title{Trends and Patterns in Time series}
\description{Time series smoothing using averaging and and Holt-Winters fitting and forecasting. Plots are provided for comparisons. Required packages: xts, forecast}
\usage{
decompositions(input)
}

\arguments{
  \item{input}{The time series to be decomposed as a single column dataframe
}
}

\value{
Plot of daily and monthly averages of the input time series. Plot of original versus fitted daily series based on Holt-Winters exponential smoothing. Holt-based one-month forecast, plotted with 80\% prediction intervals (dark shaded area) and  95\% prediction intervals (light shaded area).
}

\author{
Effie Pavlidou
}

