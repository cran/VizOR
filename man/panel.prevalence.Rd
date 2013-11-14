\name{panel.prevalence}
\alias{panel.prevalence}
\title{Treatment Prevalence vs Duration graphic}
\usage{
  panel.prevalence(x, y, prevalence, box.width = NA,
    pch = "+", pch.out = 20, alpha = 0.5, ...,
    stats = boxplot.stats, coef = 1.5, do.out = TRUE)
}
\arguments{
  \item{x}{Vector of (numeric or difftime) treatment
  durations}

  \item{y}{The treatment factor}

  \item{prevalence}{Treatment prevalence, typically an
  \code{xtabs} object calculated by, e.g., xtabs(~trt,
  data)}

  \item{box.width}{If this is specified, a more-or-less
  standard boxplot will be produced}

  \item{pch}{Plotting symbol used for boxplot median}

  \item{pch.out}{Plotting symbol for outliers (default is
  dot)}

  \item{alpha}{Boxplot transparency}

  \item{\dots}{Currently used to pass \code{key.colors}
  parameter}

  \item{stats}{Function to generate boxplot statistics
  (default is \code{boxplot.stats})}

  \item{coef}{How far boxplot whiskers should extend
  outside the box}

  \item{do.out}{Logical; should outliers be plotted?}
}
\description{
  Provides a special graphic for treatment
  prevalence-vs-duration. Used in place of lattice function
  'panel.bwplot', this function enables a special kind of
  graphic displaying prevalence of various kinds of
  treatment within a patient population, together with the
  distribution of treatment durations.
}
\author{
  David C. Norris
}
\seealso{
  \code{\link[lattice:panel.bwplot]{panel.bwplot}}
}
\keyword{'passthru'}
\keyword{##}
\keyword{a}
\keyword{bwplot.}
\keyword{Document}
\keyword{hplot}
\keyword{including}
\keyword{internal}
\keyword{kinds}
\keyword{of}
\keyword{several}
\keyword{standard}
\keyword{to}
\keyword{TODO:}
\keyword{usage,}

