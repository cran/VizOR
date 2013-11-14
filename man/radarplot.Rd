\name{radarplot}
\alias{radarplot}
\title{A multidimensional 'Table 1 at-a-glance'}
\usage{
  radarplot(x, data, datadist = getOption("datadist"),
    xlim = c(-1.4, 1.4), ylim = c(-1.2, 1.2),
    rescale = c("IQR", "range"), treatment = NULL,
    stratify = NULL, strength = NULL, include.na = FALSE,
    overall = FALSE, ...)
}
\arguments{
  \item{x}{Typically, a formula of the form \code{S~x},
  \code{S~x|t} or \code{S~x|t*u}, where \code{S} is the
  (suitably normalized) summary matrix of a
  \code{summary.formula.cross} object having factors
  \{\code{x}[, \code{t}[, \code{u}]]\} as covariates. In
  the resulting radar plot, the \code{x} factor will
  correspond to colored polygons overlaid within each panel
  of a trellis of 0, 1 or 2 dimensions, defined by the
  (optional) terms \code{t} and \code{u}.  Alternatively,
  \code{x} may be a fitted model in which the LHS is the
  experimental outcome, and the RHS includes the spoke
  variables.}

  \item{data}{Either a \code{summary.formula.cross} object
  (when \code{x} is a formula), or a \code{data.frame}
  (when \code{x} is a fitted model).}

  \item{datadist}{An \code{rms:datadist} object describing
  the underlying dataset. By suitably modifying
  \code{datadist$limits}, greater control may be achieved
  over the automatic calculation of strength-of-association
  measures.}

  \item{xlim}{A positioning parameter that really should be
  automated!}

  \item{ylim}{A positioning parameter that really should be
  automated!}

  \item{rescale}{How to rescale continuous covariates for
  plotting on the spokes}

  \item{treatment}{When \code{x} is an outcomes model,
  identifying the treatment variable (and any variables to
  stratify on, see below) permits the formula embedded in
  the model to be used to generate a
  \code{summary.formula.cross} object}

  \item{stratify}{When \code{x} is an outcomes model,
  identifying the treatment variable (see above) and any
  variables to stratify on, permits the formula embedded in
  the model to be used to generate a
  \code{summary.formula.cross} object}

  \item{strength}{When the default choice of
  strength-of-association measure is not suitable, it may
  be specified explicitly as a named vector with elements
  corresponding to the spoke variables, in the order they
  appear in the model formula. The names will be printed at
  the outer endpoints of the spokes, and will typically be
  numbers (e.g., odds ratios) rounded to 2-3 significant
  figures.}

  \item{include.na}{Should the summary include NA values?
  Passed to the summary.formula call that generates the
  radarplot data.}

  \item{overall}{Should the summary include an 'ALL' row?
  Passed to the summary.formula call that generates the
  radarplot data.}

  \item{...}{Other parameters to be passed to
  \code{panel.radarplot}}
}
\description{
  Plots a specialized radar (aka, \sQuote{star}) plot to
  compare summary statistics on a handful of covariates,
  across a multi-dimensional set of comparison groups, as
  in the standard \sQuote{Table 1} of experimental studies,
  where the groups correspond to treatment assignments.
}
\details{
  The covariates appear on the radii, or 'spokes' of the
  radarplot, while the several (up to 3) dimensions
  defining the comparison groups appear as a color-coded
  overlay, plus up to two trellis dimensions.  In generic
  applications, the client code employs
  \code{Hmisc::summary.formula(method='cross')} to create
  the multi-dimensional array for plotting, and then
  rescales the array elements by a linear or affine
  transformation to the interval [0,1].  Ordered factors
  may be treated as quasi-continuous variables according to
  their internal integer representation; logical covariates
  (and also, conveniently, 2-valued factors) appear as
  proportions; and unordered categorical variables with n
  levels may be transformed to n-1 dummy variables, which
  may then be plotted as proportions.  An optional
  \code{strength} parameter may be specified, providing a
  strength-of-association measure connecting the spoke
  variables to the experimental outcome.  This is reflected
  graphically by fanning the spokes out into sectors
  subtending angles proportional to the strength of
  associations. Used in this way, the radar plot draws
  attention to those spoke variables with greatest
  potential to confound the outcome, and so becomes a
  useful exploratory tool.  To provide convenient support
  for this specialized usage, \code{radarplot} optionally
  accepts a regression model in which the outcome of
  interest is the regressand, and the spoke variables are
  regressors.  From this model, strength-of-association
  measures are automatically derived, appropriately to the
  class of the regression model.  Invoked in this way,
  \code{radarplot} is also able to free client code from
  the burden of generating and rescaling a
  \code{summary.formula.cross} object. At this time, models
  of class \code{lrm} and \code{cph} are supported. See the
  example code below.
}
\examples{
library(rms)
df <- upData(mtcars,
             cyl=factor(cyl,levels=2*(2:4),labels=paste(2*(2:4),"cyl", sep="-")),
             am=factor(am,levels=0:1,labels=c("automatic","manual")),
             gear=factor(gear,levels=3:5,labels=paste(3:5,"speed", sep="-")),
             labels=c(
               mpg="Miles per gallon"
               ,cyl="Number of cylinders"
               ,disp="Displacement"
               ,hp="Gross horsepower"
               ,drat="Rear axle ratio"
               ,wt="Weight"
               ,qsec="1/4 mile time"
               ,am="Transmission type"
               ,gear="Number of forward gears"
               ,carb="Number of carburetors"
               ),
             units=c(
               wt="lb/1000"
               ,disp="in^3"
               ,qsec="sec"
               ),
             drop='vs'
             )
s <- summary(cbind(mpg, disp, hp, drat, wt) ~ cyl + gear + am,
             method='cross', overall=TRUE, data=df)
dd <- datadist(df)
radarplot(S ~ cyl | gear*am, data=s, datadist=dd, rescale="range")

## TODO: Provide example of convenient usage with 'lrm' and 'cph' models.
}
\author{
  David C. Norris
}
\keyword{hplot}
\keyword{htest}

