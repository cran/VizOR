\name{panel.radarplot}
\alias{panel.radarplot}
\title{Panel function for multidimensional 'Table 1 at-a-glance'
See \code{\link{radarplot}}}
\usage{
  panel.radarplot(x, y, groups = NULL,
    pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch,
    col,
    col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
    col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
    font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font,
    fontfamily = (if (is.null(groups)) plot.symbol else superpose.symbol)$fontfamily,
    fontface = (if (is.null(groups)) plot.symbol else superpose.symbol)$fontface,
    lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
    cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex,
    fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill,
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
    w = 0.12, h = 0.06, strength = NULL, ...)
}
\arguments{
  \item{x}{}

  \item{y}{}

  \item{groups}{}

  \item{pch}{}

  \item{col}{}

  \item{col.line}{}

  \item{col.symbol}{}

  \item{font}{}

  \item{fontfamily}{}

  \item{fontface}{}

  \item{lty}{}

  \item{cex}{}

  \item{fill}{}

  \item{lwd}{}

  \item{\dots}{}
}
\description{
  Panel function for multidimensional 'Table 1 at-a-glance'
  See \code{\link{radarplot}}
}
\author{
  David C. Norris
}
\seealso{
  \code{\link{radarplot}}
}
\keyword{hplot}
\keyword{internal}

