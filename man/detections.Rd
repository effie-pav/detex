\name{detections}
\alias{detections}
\title{Anomaly detection/ Change detection in time series}
\description{The script imports time series and uses different functions to perform anomaly and/or temporal change detection with different approaches. These include use of thresholds, counting flagged anomalies in rolling windows or discrete periods, calculating cumulative normalized values and using the Kolmogorov-Smirnoff test to compare the distribution of values in different temporal windows. Required packages: xts, stats}
\usage{
detections(input, lwin, k)
}

\arguments{
    \item{input}{The input values of the time series, not as a ts object
}
  \item{lwin}{The length of the temporal window (as number of observations) for KS  tests
}
  \item{k}{The step of the roll (the number of observations between the beginning of each of the two windows in rolling window KS test)
}
  \item{ntot}{The number of observations in the whole series
}
}

\value{
Plot of numbers of anomalies in a moving window of length lwin. Plot of cumulative values in a moving window of length lwin. Plot of the D-statistic, p-value and D at a 95\% level for the ks test, in discrete and in stepwise-rolling temporal windows.
}

\author{Effie Pavlidou}

