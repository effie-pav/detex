\name{normalize}
\alias{normalize}

\title{Spatiotemporal normalization
}
\description{The function normalizes the central pixel with the average of a frame of neighbouring pixels to suppress commonalities in the signal.The process is repeated in every timeslot to produce a time series of normalized values.
}
\usage{
normalize(nr, central, data)
}

\arguments{
  \item{nr}{The length of the time series (nr of observations)
}
  \item{central}{The time series of the central pixel
}
  \item{data}{The dataframe containing the time series of the pixels belonging to the normalization frame
}
}
\value{
A time series of normalized central pixel values
}
\references{
Pavlidou, E., van der Meijde, M., van der Werff, H. M. A., & Hecker, C. A. (2016). Finding a needle by removing the haystack: a spatio-temporal normalization method for geophysical data. Computers & geosciences, 90(A), 78-86. DOI: 10.1016/j.cageo.2016.02.016
}
\author{
Effie Pavlidou
}
