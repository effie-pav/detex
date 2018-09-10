\name{normbase}
\alias{normbase}

\title{Complete spatiotemporal normalization}
\description{The script imports time series, calculates normalized series, estimates input uncertainty propagation in normalized values and plots normalized series with their uncertainty.}
\usage{
normbase(central, data, uncertainties, un_central)
}

\arguments{
  \item{central}{The time series of the central pixel
}
  \item{data}{The dataframe containing time series of the normalization frame
}
  \item{uncertainties}{The dataframe containing uncertainties of the series of the frame pixels
}
  \item{un_central}{The uncertainties of the series of the central pixel
}
}

\value{
Plot of normalized values with the related uncertainty (in lightblue)
}

\author{Effie Pavlidou}
