\name{period_counts_simple}
\alias{period_counts_simple}

\title{Counting anomalies per period
}
\description{The function counts number of anomalies in three user specified periods within the pixel's time series. Requires that a calender has been constructed as an ISOdate sequence.
}
\usage{
period_counts_simple(predate, postdate, input)
}

\arguments{
  \item{predate}{predate the first date of interest in the format "yyyy-mm-dd hr:mn:sc GMT"
}
  \item{postdate}{the second date of interest in the format "yyyy-mm-dd hr:mn:sc GMT"
}
  \item{input}{the time series containing 1 for detected anomaly and NA for no detected anomaly (return of a call to the flag function)
}
}
\details{
Can be adjusted to more periods and more locations to create a dataset for ANOVA
}
\value{
Numbers of anomalies (a) before the pre-date, (b) between pre- and post-date, (c) after postdate
}
\author{
Effie Pavlidou
}
