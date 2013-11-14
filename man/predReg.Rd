\name{predReg}
\alias{predReg}
\title{Registry Ensemble Prediction}
\usage{
  predReg(genReg, N, M = 100, fit = NULL,
    adjust.to = fit$Design$limits["Adjust to", ],
    do.pred = function(df) {     fit.call <- fit$call
        fit.call$data <- quote(df)
        eval(parse(text = "predReg.fit <<- eval(fit.call)"))
        eval(parse(text =
	 "predReg.fit$Design$limits['Adjust to',names(adjust.to)]<<-adjust.to")
	)
        if (eval(parse(text = "is(predReg.fit,'lrm')"))) 
            eval(parse(text = "Predict(predReg.fit, fun=plogis)"))
        else eval(parse(text = "Predict(predReg.fit)")) },
    ...)
}
\arguments{
  \item{genReg}{A function that returns a simulated
  registry dataset, taking as its first parameter the
  desired size of the simulated registry, and possibly
  other parameters passed through via the \code{\dots} arg}

  \item{N}{Size of generated registries}

  \item{M}{Size of the ensemble}

  \item{fit}{A fitted model usually intended to serve as a
  template for a model to be fitted to the simulated
  registries.  This may be \code{NULL} when \code{do.pred}
  is provided explicitly in the call}

  \item{adjust.to}{A list of adjust-to values for the
  fitted models, defaulting to the adjust-to parameters of
  \code{fit}}

  \item{do.pred}{A function to be run on the data generated
  during each iteration of the simulation, generating
  either an \code{rms:Predict} object, or a (usually,
  named) atomic vector}

  \item{...}{Additional parameters passed to \code{genReg}}
}
\value{
  Depending on the return type of \code{do.pred}, either an
  \code{rms:Predict} object containing ensemble-averaged
  predictions with confidence bounds reflecting their
  estimated ensemble variance, or else a data frame
  collecting the vector returned by \code{do.pred}
}
\description{
  Generate an \code{rms:Predict} object or data frame for
  an ensemble of simulated disease registries
}
\details{
  Given a function for generating a simulated disease
  registry, this function generates an ensemble of such
  registries. It then returns an \code{rms:Predict} object
  that contains ensemble-averaged predictions and
  confidence bounds.
}
\author{
  David C. Norris
}
\keyword{datagen}

