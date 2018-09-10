\name{flag}
\alias{flag}

\title{Anomaly flagging}
\description{The function detects values that exceed a man+2sigma threshold and flags them as anomalies ("1"). Values not exceeding the threshold are replaced with NA}
\usage{
flag(input, nr)
}
\arguments{
  \item{input}{The input time series
}
\item{nr}{nr The length of input time series (total number of observations)
}
}

\value{
Returns a vector of 1 (anomalies) and NA (values not exceeding the threshold)
}

\author{
Effie Pavlidou
}
