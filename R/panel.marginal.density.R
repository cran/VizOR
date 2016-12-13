##' Panel function for supplementing scatter plots with marginal density graphics
##'
##' This panel function is adapted directly from lattice::panel.densityplot,
##' and aims to make inclusion of optional density graphics on either or both
##' axes of x-y scatter plots.
##' @title panel.marginal.density
##' @param x 
##' @param y 
##' @param darg
##' @param plot.points
##' @param groups
##' @param weights
##' @param jitter.amount
##' @param type
##' @param ... 
##' @param identifier
##' @param intrude
##' @return None
##' @author David C. Norris
##' @seealso \code{\link{panel.densityplot}}
##' @keywords internal hplot
##' @export panel.marginal.density
panel.marginal.density <-
    function (x, y, darg = list(n = 30), plot.points = FALSE,
              groups = NULL, weights = NULL, jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
              type = "p", ..., identifier = "density", intrude = 0.1)
{
    ## The aim here is to produce bivariate marginal densities for the x and y axes.
    ## The code is adapted directly from lattice::panel.densityplot.
    ## Note that the marginal densities may be suppressed individually
    ## by setting either x=NA or y=NA.

    if (!is.null(groups)) {
        panel.superpose(x, y, darg = darg, plot.points = plot.points, 
                        ref = FALSE, groups = groups, weights = weights, 
                        panel.groups = panel.marginal.density, jitter.amount = jitter.amount, 
                        type = type, ..., intrude = intrude)
    }
    else {
        ## TODO: Check that this code is correct
        ## TODO: Ensure the y-axis marginal rugs are also supported
        switch(as.character(plot.points),
               rug = {
                   panel.rug(x = x, x.units = c("npc", "native"),
                             type = type, ..., identifier = paste(identifier, "rug"))
                   panel.rug(y = y, y.units = c("npc", "native"),
                             type = type, ..., identifier = paste(identifier, "rug"))
               },
               ## TODO: Correctly implement for the `TRUE` and 'jitter' options, too:
               `TRUE` = panel.xyplot(x = x, y = rep(0, length(x)),
                   type = type, ..., identifier = identifier), 
               jitter = panel.xyplot(x = x, y = jitter(rep(0, length(x)), amount = jitter.amount),
                   type = type, ..., identifier = identifier))
        density.fun <- function(x, weights, subscripts = TRUE, darg, ...) {
            do.call("density", c(list(x = x, weights = weights[subscripts]), darg))
        }
        if (sum(!is.na(x)) > 1) {
            h <- density.fun(x = x, weights = weights, ..., darg = darg)
            lim <- current.panel.limits()$xlim
            id <- h$x > min(lim) & h$x < max(lim)
            panel.height <- current.panel.limits()$ylim %*% c(-1, 1)
            scale.height <- intrude * panel.height / max(h$y) # TODO: Use *visible* max(h$y[id]) instead?
            panel.lines(x = h$x[id], y = h$y[id]*scale.height + current.panel.limits()$ylim[1],
                        ..., identifier = paste(identifier, "marginal-x"))
        }
        ## Draw the y-axis marginal density, too
        if (sum(!is.na(y)) > 1) {
            h <- density.fun(x = y, weights = weights, ..., darg = darg)
            lim <- current.panel.limits()$ylim
            id <- h$x > min(lim) & h$x < max(lim)
            panel.width <- current.panel.limits()$xlim %*% c(-1, 1)
            scale.width <- intrude * panel.width / max(h$y)
            panel.lines(x = h$y[id]*scale.width + current.panel.limits()$xlim[1], y = h$x[id],
                        ..., identifier = paste(identifier, "marginal-y"))
        }
    }
}
