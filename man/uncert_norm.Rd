\name{uncert_norm}
\alias{uncert_norm}

\title{Uncertainty propagation
}
\description{The function applies a Taylor-based method to calculate frame uncertainty given the LST values of frame pixels and central pixel, and LST product-specified uncertainties for frame pixels and for central pixel time series.
}
\usage{
uncert_norm(data, uncertainties, nr, central, un_central)
}

\arguments{
  \item{data}{The dataframe containing time series of the normalization frame
}
  \item{central}{The time series of the central pixel
}
  \item{un_central}{The uncertainties of the series of the central pixel
}
  \item{uncertainties}{The dataframe containing uncertainties of the series of the frame pixels
}
  \item{nr}{length of the time series (including slots without available measurements)
}
}
\details{
Uncertainty calculations in the frame consider (a) spatially correlated uncertainty and (b) sampling uncertainty
}
\value{
Time series containing the uncertainty estimate of each normalized value of the input series
}
\references{
Heuvelink, G.B.M., 1999. Propagation of error in spatial modelling with GIS. In: Geographical Information Systems. Volume 1, 2nd Edition. Longley, P.A., Goodchild, M.F., Maguire, D.J., Rhind, D.W. (Eds). John Wiley & Sons, Inc. pp. 207-217
}
\author{
Effie Pavlidou
}
