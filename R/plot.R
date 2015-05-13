
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


#' @title Obtain Transparent Colors
#'
#' @description Returns an RGB transparent color from any standard R color.
#'
#' @param col Vector consisting of colors, of any of the three kinds of
#'        \code{R} color specifications (named, hexadecimal, or integer; see
#'        \code{\link{col2rgb}}).
#' @param alpha Vector of transparency levels, where 0 is transparent and 1
#'        is opaque.
#' @param invisible Supresses printing of the RGB color.
#'
#' @details
#' The purpose of this function is to facilitate color transparency, which is
#' used widely in \code{EpiModel} plots. This is an internal function that is
#' not ordinarily called by the end-user. This function allows that one of col
#' or alpha may be of length greater than 1.
#'
#' @return
#' A vector of length equal to the input \code{col} vector or the \code{alpha},
#' vector, if one or the other is of length greater than 1, containing the
#' transformed color values in hexidemical format.
#'
#' @seealso \code{\link{rgb}}, \code{\link{col2rgb}}
#'
#' @export
#' @keywords internal
#'
transco <- function(col, alpha = 1, invisible = FALSE) {

  if (length(alpha) > 1 && length(col) > 1) {
    stop("Length of col or length of alpha must be 1", call. = FALSE)
  }

  if (alpha > 1 || alpha < 0) {
    stop("Specify alpha between 0 and 1", call. = FALSE)
  }

  newa <- floor(alpha * 255)
  t1 <- col2rgb(col, alpha = FALSE)
  t2 <- rep(NA, length(col))

  if (length(col) > 1) {
    for (i in seq_along(col)) {
      t2[i] <- rgb(t1[1, i], t1[2, i], t1[3, i], newa, maxColorValue = 255)
    }
  }
  if (length(alpha) > 1) {
    for (i in seq_along(alpha)) {
      t2[i] <- rgb(t1[1, 1], t1[2, 1], t1[3, 1], newa[i], maxColorValue = 255)
    }
  }
  if (length(col) == 1 && length(alpha) == 1) {
    t2 <- rgb(t1[1, 1], t1[2, 1], t1[3, 1], newa, maxColorValue = 255)
  }

  if (invisible == TRUE) {
    invisible(t2)
  } else {
    return(t2)
  }
}
