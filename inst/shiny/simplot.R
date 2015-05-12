
# setwd("~/Dropbox/R/Dev/statnet_commons/EpiModel/trunk/apps/shiny/conc.microsim")
# md = 0.75
# dur = 12
# conc = "TT"

transco <- function(col, 
                    alpha=1, 
                    invisible=TRUE
                    ) {
  
  if (alpha > 1 || alpha < 0)
    stop("Specify alpha between 0 and 1")
  
  newa <- floor(alpha*255)
  t1 <- col2rgb(col, alpha = FALSE)
  t2 <- rep(NA, length(col))
  
  for (i in 1:length(col)) {
    t2[i] <- rgb(t1[1,i], t1[2,i], t1[3,i], newa, maxColorValue=255)
  }
  
  if (invisible == TRUE) {
    invisible(t2)
  } else {
    return(t2)
  }
}

sim.plot <- function(md, 
                     dur, 
                     conc, 
                     medians=FALSE,
                     sims=NULL, 
                     simlines=TRUE, 
                     alpha=NULL,
                     leg=TRUE) {
  
  data <- paste0('data/sim.', conc, '.', md*100, '.', dur, '.Rdata')
  load(data)
  
  par(mar=c(3.5,3.5,2.5,1), mgp=c(2.2,1,0))
  
  mains <-   switch(conc,
                    "TT" = "No concurrency",
                    "FT" = "Female only concurrency",
                    "TF" = "Male only concurrency",
                    "FF" = "Both sexes concurrency")
    
    if (is.null(sims)) sims <- dim(out)[2]
    
    data.m <- out[,,2]
    if (sims < 50) 
      data.m <- data.m[,sample(1:50, sims)]
    data.f <- out[,,1]
    if (sims < 50) 
      data.f <- data.f[,sample(1:50, sims)]
    
    if (sims > 1) {
      med.m <- apply(data.m, 1, median)
      med.f <- apply(data.f, 1, median)
    } else {
      med.m <- data.m
      med.f <- data.f
    }
    
    pal <- c(brewer.pal(3, 'Set1'), 'black')
    if (is.null(alpha)) alpha <- min(5/sims, 1)
    tpal <- transco(pal, alpha)
    
    numtimesteps <- nrow(data.m)
    ymax <- max(out)*1.2
  
    plot(1:numtimesteps, type='n', ylim=c(0,ymax), 
         xlab='Months', ylab='Prevalence', main=mains)
    if (simlines == TRUE) {
      if (sims == 1) {
        lines(data.m, col=tpal[1], lwd=0.5)
        lines(data.f, col=tpal[2], lwd=0.5)
      } else {
        for (sim in 1:sims){
          lines(data.m[,sim], col=tpal[1], lwd=0.5)
          lines(data.f[,sim], col=tpal[2], lwd=0.5)
        }
      }
    }
    if (medians == TRUE) {
      lines(med.m, col='black', lwd=2.3)
      lines(med.m, col=pal[1], lwd=2)
      
      lines(med.f, col='black', lwd=2.3)
      lines(med.f, col=pal[2], lwd=2)
    }
    if (leg == TRUE) {
      legend('topleft', c('Male','Female'), lwd=3, col=pal[1:2], bty='n')
    }
    
}


sim.table <- function(md, 
                      dur,  
                      male=1, 
                      digits=4
                      ) {
  
  data <- paste0('data/sim.', conc, '.', md*100, '.', dur, '.Rdata')
  load(data)
  
  fn <- paste0('data/sim.TT.', md*100, '.', dur, '.Rdata')
  fn[2] <- paste0('data/sim.FT.', md*100, '.', dur, '.Rdata')
  fn[3] <- paste0('data/sim.TF.', md*100, '.', dur, '.Rdata')
  fn[4] <- paste0('data/sim.FF.', md*100, '.', dur, '.Rdata')
  for (i in 1:4) {
    load(fn[i])
    assign(paste0('d', i), out)
  }
    

  
  d1.med.f <- apply(d1[,,1], 1, median)
  d1.med.m <- apply(d1[,,2], 1, median)
  d2.med.f <- apply(d2[,,1], 1, median)
  d2.med.m <- apply(d2[,,2], 1, median)
  d3.med.f <- apply(d3[,,1], 1, median)
  d3.med.m <- apply(d3[,,2], 1, median)
  d4.med.f <- apply(d4[,,1], 1, median)
  d4.med.m <- apply(d4[,,2], 1, median)
  
  times <- c(1, seq(500, 3000, 500))
  if (male == 1) {
    d1.out.m <- d1.med.m[times]
    d2.out.m <- d2.med.m[times]
    d3.out.m <- d3.med.m[times]
    d4.out.m <- d4.med.m[times]
    resmat <- rbind(d1.out.m, d2.out.m, d3.out.m, d4.out.m)
  } else {
    d1.out.f <- d1.med.f[times]
    d2.out.f <- d2.med.f[times]
    d3.out.f <- d3.med.f[times]
    d4.out.f <- d4.med.f[times]
    resmat <- rbind(d1.out.f, d2.out.f, d3.out.f, d4.out.f)
  }
  
  mains <- c("No Conc.",
             "Female Conc.",
             "Male Conc.",
             "Both Conc.")
  rownames(resmat) <- mains
  colnames(resmat) <- times
  
  resmat <- round(resmat, digits)  
  
  return(resmat)
  
}