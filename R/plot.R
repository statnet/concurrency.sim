
#' @title Plot Values from a Concurrency Microsimulation Model
#'
#' @description Plots values from an concurrency microsimulation
#'              epidemic model simulated with \code{conc_microsim}.
#'
#' @param x Object of class \code{conc_microsim}.
#' @param sim.lines If \code{TRUE}, plot individual simulation lines.
#' @param sim.alpha Transparency level for simulation lines, where 0 = transparent
#'        and 1 = opaque (see \code{transco} in the \code{EpiModel} package).
#' @param mean.line If \code{TRUE}, plot the row means of the simulation lines.
#' @param mean.smooth If \code{TRUE}, smooth the mean line.
#' @param mean.lwd Line width for mean line.
#' @param qnts If \code{TRUE}, plot quantile polygon for specified range.
#' @param qnt.alpha Transparency level for quantile band, similiarly specified to
#'        \code{sim.alpha}.
#' @param xlim x-axis scale limits for plot, with default based on model time steps.
#' @param ylim y-axis scale limits for plot, with default based on range of
#'        simulated data.
#' @param ... Additional arguments (not used)
#' @details
#' This function plots the disease prevalence from an \code{conc_microsim}
#' model. The function is currently limited to individual simulation lines of
#' disease prevalence.
#'
#' @method plot conc_microsim
#' @export
#'
plot.conc_microsim <- function(x,
                               sim.lines = FALSE,
                               sim.alpha = 0.5,
                               mean.line = TRUE,
                               mean.smooth = TRUE,
                               mean.lwd = 3,
                               qnts = 0.5,
                               qnt.alpha = 0.3,
                               xlim,
                               ylim,
                               ...) {

  nsims <- length(x)
  nsteps <- nrow(x[[1]])
  pal <- wesanderson::wes_palette("Zissou", 5)[c(5, 1)]
  sim.pal <- transco(pal, sim.alpha)
  qnt.pal <- transco(pal, qnt.alpha)

  if (missing(ylim)) {
    ylim <- c(0, min(1, max(sapply(x, max)) * 1.25))
  }
  if (missing(xlim)) {
    xlim <- c(0, nsteps)
  }

  plot(1, 1, type = "n", xlim = xlim, ylim = ylim, bty = "n",
       xlab = "Time (months)", ylab = "Infected")

  if (qnts > 0) {
    draw_qnts(x, qnts, qnt.pal)
  }
  if (sim.lines == TRUE) {
    for (i in 1:nsims) {
      lines(1:nsteps, x[[i]][,"femlPrev"], col = sim.pal[1], lwd = 0.8)
      lines(1:nsteps, x[[i]][,"malePrev"], col = sim.pal[2], lwd = 0.8)
    }
  }
  if (mean.line == TRUE) {
    draw_means(x, mean.smooth, mean.lwd = mean.lwd, pal)
  }


  legend("topleft", c("Female", "Male"), lwd = 4, col = pal[1:2],
         bty = "n", cex = 0.9)

}


draw_means <- function(x, mean.smooth, mean.lwd, pal) {

  df <- do.call("cbind", x)
  dfF <- df[, which(colnames(df) == "femlPrev")]
  dfM <- df[, which(colnames(df) == "malePrev")]

  meanF <- rowMeans(dfF)
  meanM <- rowMeans(dfM)

  if (mean.smooth == TRUE) {
    meanF <- supsmu(x = 1:length(meanF), y = meanF)$y
    meanM <- supsmu(x = 1:length(meanM), y = meanM)$y
  }

  lines(meanF, lwd = mean.lwd, col = pal[1])
  lines(meanM, lwd = mean.lwd, col = pal[2])

}


draw_qnts <- function(x, qnts, qnt.pal) {

  df <- do.call("cbind", x)
  dfF <- df[, which(colnames(df) == "femlPrev")]
  dfM <- df[, which(colnames(df) == "malePrev")]

  quants <- c((1 - qnts) / 2, 1 - ((1 - qnts) / 2))
  qntF <- apply(dfF, 1, function(x) quantile(x, c(quants[1], quants[2]), na.rm = TRUE))
  qntM <- apply(dfM, 1, function(x) quantile(x, c(quants[1], quants[2]), na.rm = TRUE))

  xxF <- c(1:(ncol(qntF)), (ncol(qntF)):1)
  yyF <- c(qntF[1, ], rev(qntF[2, ]))
  polygon(xxF, yyF, col = qnt.pal[1], border = NA)

  xxM <- c(1:(ncol(qntM)), (ncol(qntM)):1)
  yyM <- c(qntM[1, ], rev(qntM[2, ]))
  polygon(xxM, yyM, col = qnt.pal[2], border = NA)

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
