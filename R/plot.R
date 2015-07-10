
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
#' @param qnts.smooth If \code{TRUE}, smooth the quantile band.
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
plot.conc_microsim <- function(x, sim.lines = FALSE, sim.alpha = 0.5,
                               mean.line = TRUE, mean.smooth = TRUE,
                               mean.lwd = 3, qnts = 0.5, qnt.alpha = 0.3,
                               qnts.smooth = TRUE, xlim, ylim, ...) {

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
    draw_qnts(x, qnts, qnt.pal, qnts.smooth)
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


draw_qnts <- function(x, qnts, qnt.pal, qnts.smooth) {

  df <- do.call("cbind", x)
  dfF <- df[, which(colnames(df) == "femlPrev")]
  dfM <- df[, which(colnames(df) == "malePrev")]

  quants <- c((1 - qnts) / 2, 1 - ((1 - qnts) / 2))
  qntF <- apply(dfF, 1, function(x) quantile(x, c(quants[1], quants[2]), na.rm = TRUE))
  qntM <- apply(dfM, 1, function(x) quantile(x, c(quants[1], quants[2]), na.rm = TRUE))

  xxF <- c(1:(ncol(qntF)), (ncol(qntF)):1)
  if (qnts.smooth == FALSE) {
    yyF <- c(qntF[1, ], rev(qntF[2, ]))
  } else {
    yyF <- c(supsmu(x = 1:(ncol(qntF)), y = qntF[1, ])$y,
             rev(supsmu(x = 1:(ncol(qntF)), y = qntF[2, ])$y))
  }
  polygon(xxF, yyF, col = qnt.pal[1], border = NA)

  xxM <- c(1:(ncol(qntM)), (ncol(qntM)):1)
  if (qnts.smooth == FALSE) {
    yyM <- c(qntM[1, ], rev(qntM[2, ]))
  } else {
    yyM <- c(supsmu(x = 1:(ncol(qntM)), y = qntM[1, ])$y,
             rev(supsmu(x = 1:(ncol(qntM)), y = qntM[2, ])$y))
  }
  polygon(xxM, yyM, col = qnt.pal[2], border = NA)

}
