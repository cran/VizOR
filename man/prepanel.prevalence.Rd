\name{prepanel.prevalence}
\alias{prepanel.prevalence}
\title{Adapts y-axis limits to prevalence range and any nonzero box width}
\usage{
  prepanel.prevalence(x, y, ...)
}
\arguments{
  \item{x}{Vector of (numeric or difftime) treatment
  durations}

  \item{y}{The treatment factor}

  \item{\dots}{Used to pass \code{prevalence} and
  \code{box.width} parameters}
}
\description{
  When generating a \sQuote{Prevalence Plot} via
  \code{bwplot(panel=panel.prevalence)}, specifying
  \code{prepanel=prepanel.prevalence} spares one from
  calculating the y-axis bounds.
}
\author{
  David C. Norris
}
\seealso{
  \code{\link{panel.prevalence}}
}
\keyword{hplot}

