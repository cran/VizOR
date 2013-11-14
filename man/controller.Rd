\name{controller}
\alias{controller}
\title{Provides a GUI panel offering dynamic control of the adjust-to settings
of the partial effects plot of an rms fit object.}
\usage{
  controller(fit, datadist = NULL)
}
\arguments{
  \item{fit}{The \code{rms} fit object to be visualized}

  \item{datadist}{Optionally, a \code{datadist} for the
  fitted object may be provided, conveniently enabling use
  of this function with models in which logical covariates
  are not typed as such, but represented in {0,1}, or in
  which covariates more properly coded as ordered factors
  are represented instead as a finite number of integer
  values.}
}
\value{
  A controller panel is popped up on the screen, along with
  a partial effects plot for the \code{fit} object. No
  value is returned.
}
\description{
  The parameters are identified automatically by inspection
  of the fit object, and widgets appropriate to their data
  types are chosen automatically.
}
\author{
  David C. Norris
}
\keyword{dynamic}
\keyword{iplot}

