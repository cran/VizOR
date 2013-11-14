\name{remvar}
\alias{remvar}
\title{remvar}
\usage{
  remvar(f, vars)
}
\arguments{
  \item{f}{A model formula}

  \item{vars}{A character vector giving names of variables
  to remove from \code{f}}
}
\value{
  A modified formula, omitting the specified variables
}
\description{
  Removes variables from a regression model formula
}
\details{
  This is a utility function, intended to support
  \code{fastbw}
}
\author{
  David C. Norris
}

