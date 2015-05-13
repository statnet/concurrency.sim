
#' @title Plot Values from a Concurrency Microsimulation Model
#'
#' @description Plots values from an concurrency microsimulation
#'              epidemic model simulated with \code{conc_microsim}.
#'
#' @param x an object of class \code{conc_microsim}.
#' @param xlim x-axis scale limits for plot, with default based on model time steps.
#' @param ylim y-axis scale limits for plot, with default based on range of
#'        simulated data.
#' @param alpha transparency level for simulation lines, where 0 = transparent
#'        and 1 = opaque (see \code{transco} in the \code{EpiModel} package).
#' @param lwd line width for simulation lines.
#' @param ... additional arguments to pass to main plot window (see
#'        \code{\link{plot.default}}).
#'
#' @details
#' This function plots the disease prevalence from an \code{conc_microsim}
#' model. The function is currently limited to individual simulation lines of
#' disease prevalence.
#'
#' @method plot conc_microsim
#' @export
#'
plot.conc_microsim <- function(x,
                               xlim,
                               ylim,
                               alpha = 1,
                               lwd = 1,
                               ...) {

  nsims <- length(x)
  nsteps <- nrow(x[[1]])
  pal <- transco(wesanderson::wes_palette("Zissou", 5), alpha)[c(5, 1)]

  if (missing(ylim)) {
    ylim <- c(0, max(sapply(x, max)) * 1.2)
  }
  if (missing(xlim)) {
    xlim <- c(0, nsteps)
  }

  plot(1, 1, type = "n", xlim = xlim, ylim = ylim, bty = "n",
       xlab = "Time (months)", ylab = "Infected", ...)
  for (i in 1:nsims) {
    lines(1:nsteps, x[[i]][,1], col = pal[1], lwd = lwd)
    lines(1:nsteps, x[[i]][,2], col = pal[2], lwd = lwd)
  }
  legend("topleft", c("Female", "Male"), lwd = 3, col = pal[1:2],
         bty = "n", cex = 0.9)

}
