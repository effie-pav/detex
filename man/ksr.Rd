\name{ksr}
\alias{ksr}
\title{Kolmogorov-Smirnoff two-sample test in stepwise-rolling temporal windows
}
\description{The function is based on the ks.test function of the stats package (required). It tests if the subseries in a temporal window y and in its previous window x come from the same empirical distribution. It calculates the D-statistic at level 0.05 as threshold.
}
\usage{
ks(input, lwin)
}
\arguments{
  \item{input}{The input values of the time series, not as a ts object
}
  \item{lwin}{The length of the temporal window
}
  \item{k}{The step of the roll (the number of observations between the beginning of each of the two windows)
}
}

\value{The function returns D-statistic time series for each window and plots D-statistics with the threshold and p-values.
}
\references{
\code{\link{https://www.rdocumentation.org/packages/dgof/versions/1.2/topics/ks.test}}
}
\author{
Effie Pavlidou
}
\note{
 For calculation of the threshold, c(a)=1.36 for level a=0.05, c=1.95 for a=0.001. With D-stat>D95$\%$ and p-value small, the samples come from different distributions.
}
