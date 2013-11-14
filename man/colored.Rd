\name{colored}
\alias{colored}
\title{Create a colored factor,
an object of class \code{c('colored','factor')} or \code{c('colored','ordered')},
from a character vector or factor \code{x}.}
\usage{
  colored(x, color.key, ordered = is.ordered(x),
    default = NA)
}
\arguments{
  \item{x}{A character vector or factor}

  \item{color.key}{A named vector or list mapping factor
  levels (the names) to colors (the values). If missing (as
  is likely in initial exploratory analyses), this is
  generated automatically as a convenience to the user. In
  case an ordered factor is generated, the order of the
  levels is determined by the ordering in
  \code{color.key}.}

  \item{ordered}{A logical value stating whether the factor
  should be ordered}

  \item{default}{An optional argument; when provided, it
  gives a string in names(color.key) to which all
  non-matching values of x should be mapped.  The effect
  will be to collect possibly many factor levels under a
  single key entry.  A common choice might be, for example,
  \code{default='OTHER'}.}
}
\value{
  An object of class
  \code{c('colored',['ordered',]'factor')}
}
\description{
  Colored factors permit persistent association of color to
  a factor, supporting consistent graphical treatment
  across multiple plots.
}
\examples{
# This is an example of de novo construction of a colored factor
weekdays <- colored(c("Mon","Tue","Wed","Thu","Fri"),
                    color.key=c(Mon="blue",Tue="red",Wed="yellow",
                                Thu="purple", Fri="green"))
# This demonstrates how one might use the 'colored' constructor
# to expand the level set of an existing factor.
week <- colored(weekdays,
                color.key=c(Sun="white", key(weekdays), Sat="gray"),
                ordered=TRUE)

# Note that 'droplevels.factor' works fine on colored factors
levels(week)
levels(droplevels(week))
}
\author{
  David C. Norris
}
\keyword{category}
\keyword{classes}

