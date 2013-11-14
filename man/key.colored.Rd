\name{key.colored}
\alias{key.colored}
\title{Extract the color key from a colored factor}
\usage{
  \method{key}{colored} (x, ...)
}
\arguments{
  \item{x}{A colored factor}

  \item{\dots}{Additional arguments (unused)}
}
\value{
  A color key, in the form of a named vector
}
\description{
  Return a colored factor's key, mapping its levels to
  their colors. Returns a named vector, mapping the factor
  levels (names) to their associated colors (values).
  Useful for constructing plot keys.
}
\author{
  David C. Norris
}
\seealso{
  \code{\link{colored}}
}
\keyword{category}
\keyword{color}

