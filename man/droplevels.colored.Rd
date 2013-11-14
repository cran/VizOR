\name{droplevels.colored}
\alias{droplevels.colored}
\title{Drop unused levels from a colored factor}
\usage{
  \method{droplevels}{colored} (x, ...)
}
\arguments{
  \item{x}{a colored factor}

  \item{...}{further arguments passed to methods}
}
\value{
  a colored factor with no unused levels
}
\description{
  Drop unused levels from a colored factor and its color
  key
}
\details{
  This subclass method extends \code{droplevels.factor} to
  handle updating of the special color key attribute of
  colored factors.
}
\author{
  David C. Norris
}

