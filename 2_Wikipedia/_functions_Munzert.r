# additional functions

char <- function(x) {as.character(x)}
num <- function(x) {as.numeric(x)}


# Calculates the geodesic distance between two points specified by radian latitude/longitude using the Haversine formula (hf); taken from http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}


## Get the labels aligned consistently around the edge of the circle
## for any n of nodes.
## This code borrows bits of ggplot2's polar_coord function
## start = offset from 12 o'clock in radians
## direction = 1 for clockwise; -1 for anti-clockwise.
## from https://stackoverflow.com/questions/23209802/placing-vertex-label-outside-a-circular-layout-in-igraph
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}


# get name of object
getObjectName <- function(v1) {
  deparse(substitute(v1))
}

# check whether try produced an error
is.error <- function(x) inherits(x, "try-error")

# draw circles on maps
# http://stackoverflow.com/questions/23071026/drawing-circle-on-r-map
plotCircle <- function(LonDec, LatDec, Km, col, border, lty) {#Corrected function
  #LatDec = latitude in decimal degrees of the center of the circle
  #LonDec = longitude in decimal degrees
  #Km = radius of the circle in kilometers
  ER <- 6371 #Mean Earth radius in kilometers. Change this to 3959 and you will have your function working in miles.
  AngDeg <- seq(1:360) #angles in degrees 
  Lat1Rad <- LatDec*(pi/180)#Latitude of the center of the circle in radians
  Lon1Rad <- LonDec*(pi/180)#Longitude of the center of the circle in radians
  AngRad <- AngDeg*(pi/180)#angles in radians
  Lat2Rad <-asin(sin(Lat1Rad)*cos(Km/ER)+cos(Lat1Rad)*sin(Km/ER)*cos(AngRad)) #Latitude of each point of the circle rearding to angle in radians
  Lon2Rad <- Lon1Rad+atan2(sin(AngRad)*sin(Km/ER)*cos(Lat1Rad),cos(Km/ER)-sin(Lat1Rad)*sin(Lat2Rad))#Longitude of each point of the circle rearding to angle in radians
  Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  polygon(Lon2Deg,Lat2Deg,col=col, border=border,lty=lty)
}




# adapted pairs() function to be able to set xlims and ylims in lower panels; taken from
#http://stackoverflow.com/questions/22810309/pairs-specifying-axes-limits-of-the-subpanels
my.pairs <- function (x, labels, panel = points, ..., lower.panel = panel, 
                      upper.panel = panel, diag.panel = NULL, text.panel = textPanel, 
                      label.pos = 0.5 + has.diag/3, line.main = 3, cex.labels = NULL, 
                      font.labels = 1, row1attop = TRUE, gap = 1, log = "", xlim=NULL, ylim=NULL) 
{
  if (doText <- missing(text.panel) || is.function(text.panel)) 
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x, 
                                                                 y, txt, cex = cex, font = font)
  localAxis <- function(side, x, y, xpd, bg, col = NULL, main, 
                        oma, ...) {
    xpd <- NA
    if (side%%2L == 1L && xl[j]) 
      xpd <- FALSE
    if (side%%2L == 0L && yl[i]) 
      xpd <- FALSE
    if (side%%2L == 1L) 
      Axis(x, side = side, xpd = xpd, ...)
    else Axis(y, side = side, xpd = xpd, ...)
  }
  localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
  localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
  localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
  localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
  dots <- list(...)
  nmdots <- names(dots)
  if (!is.matrix(x)) {
    x <- as.data.frame(x)
    for (i in seq_along(names(x))) {
      if (is.factor(x[[i]]) || is.logical(x[[i]])) 
        x[[i]] <- as.numeric(x[[i]])
      if (!is.numeric(unclass(x[[i]]))) 
        stop("non-numeric argument to 'pairs'")
    }
  }
  else if (!is.numeric(x)) 
    stop("non-numeric argument to 'pairs'")
  panel <- match.fun(panel)
  if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
    lower.panel <- match.fun(lower.panel)
  if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
    upper.panel <- match.fun(upper.panel)
  if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel)) 
    diag.panel <- match.fun(diag.panel)
  if (row1attop) {
    tmp <- lower.panel
    lower.panel <- upper.panel
    upper.panel <- tmp
    tmp <- has.lower
    has.lower <- has.upper
    has.upper <- tmp
  }
  nc <- ncol(x)
  if (nc < 2) 
    stop("only one column in the argument to 'pairs'")
  if (doText) {
    if (missing(labels)) {
      labels <- colnames(x)
      if (is.null(labels)) 
        labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels)) 
      doText <- FALSE
  }
  oma <- if ("oma" %in% nmdots) 
    dots$oma
  main <- if ("main" %in% nmdots) 
    dots$main
  if (is.null(oma)) 
    oma <- c(4, 4, if (!is.null(main)) 6 else 4, 4)
  opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
  on.exit(par(opar))
  dev.hold()
  on.exit(dev.flush(), add = TRUE)
  xl <- yl <- logical(nc)
  if (is.numeric(log)) 
    xl[log] <- yl[log] <- TRUE
  else {
    xl[] <- grepl("x", log)
    yl[] <- grepl("y", log)
  }
  for (i in if (row1attop) 
    1L:nc
    else nc:1L) for (j in 1L:nc) {
      l <- paste0(ifelse(xl[j], "x", ""), ifelse(yl[i], "y", 
                                                 ""))
      if (is.null(xlim) & is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l)
      if (is.null(xlim) & !is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l, ylim=ylim[j,i,])
      if (!is.null(xlim) & is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l, xlim = xlim[j,i,])
      if (!is.null(xlim) & !is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l, xlim = xlim[j,i,], ylim=ylim[j,i,])
      
      if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
        box()
        if (i == 1 && (!(j%%2L) || !has.upper || !has.lower)) 
          localAxis(1L + 2L * row1attop, x[, j], x[, i], 
                    ...)
        if (i == nc && (j%%2L || !has.upper || !has.lower)) 
          localAxis(3L - 2L * row1attop, x[, j], x[, i], 
                    ...)
        if (j == 1 && (!(i%%2L) || !has.upper || !has.lower)) 
          localAxis(2L, x[, j], x[, i], ...)
        if (j == nc && (i%%2L || !has.upper || !has.lower)) 
          localAxis(4L, x[, j], x[, i], ...)
        mfg <- par("mfg")
        if (i == j) {
          if (has.diag) 
            localDiagPanel(as.vector(x[, i]), ...)
          if (doText) {
            par(usr = c(0, 1, 0, 1))
            if (is.null(cex.labels)) {
              l.wid <- strwidth(labels, "user")
              cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
            }
            xlp <- if (xl[i]) 
              10^0.5
            else 0.5
            ylp <- if (yl[j]) 
              10^label.pos
            else label.pos
            text.panel(xlp, ylp, labels[i], cex = cex.labels, 
                       font = font.labels)
          }
        }
        else if (i < j) 
          localLowerPanel(as.vector(x[, j]), as.vector(x[, 
                                                         i]), ...)
        else localUpperPanel(as.vector(x[, j]), as.vector(x[, 
                                                            i]), ...)
        if (any(par("mfg") != mfg)) 
          stop("the 'panel' function made a new plot")
      }
      else par(new = FALSE)
    }
  if (!is.null(main)) {
    font.main <- if ("font.main" %in% nmdots) 
      dots$font.main
    else par("font.main")
    cex.main <- if ("cex.main" %in% nmdots) 
      dots$cex.main
    else par("cex.main")
    mtext(main, 3, line.main, outer = TRUE, at = 0.5, cex = cex.main, 
          font = font.main)
  }
  invisible(NULL)
}




# code for plotting ranks, 
# taken from http://stackoverflow.com/questions/25781284/simplest-way-to-plot-changes-in-ranking-between-two-ordered-lists-in-r
plotRanks <- function(a, b, labels.offset=0.1, arrow.len=0.1)
{
  old.par <- par(mar=c(1,1,1,1))
  
  # Find the length of the vectors
  len.1 <- length(a)
  len.2 <- length(b)
  
  # Plot two columns of equidistant points
  plot(rep(1, len.1), 1:len.1, pch=20, cex=0.8, 
       xlim=c(0, 3), ylim=c(0, max(len.1, len.2)),
       axes=F, xlab="", ylab="") # Remove axes and labels
  points(rep(2, len.2), 1:len.2, pch=20, cex=0.8)
  
  # Put labels next to each observation
  text(rep(1-labels.offset, len.1), 1:len.1, a)
  text(rep(2+labels.offset, len.2), 1:len.2, b)
  
  # Now we need to map where the elements of a are in b
  # We use the match function for this job
  a.to.b <- match(a, b)
  
  # Now we can draw arrows from the first column to the second
  arrows(rep(1.02, len.1), 1:len.1, rep(1.98, len.2), a.to.b, 
         length=arrow.len, angle=20)
  par(old.par)
}


# helper function for formattable
my_color_bar <- function (color = "lightgray", fixedWidth=150,...)
{
  formatter("span", style = function(x) style(display = "inline-block",
                                              direction = "rtl", `border-radius` = "4px", `padding-right` = "2px", `font-size` = 1,
                                              `background-color` = csscolor(color), width = paste(fixedWidth*proportion(x),"px",sep=""),
                                              ...))
}


# export rankings table; see https://github.com/renkun-ken/formattable/issues/26
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}


# compute age
get_age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}


# degrees to radians
# source: https://stackoverflow.com/questions/21402259/radian-measure-in-sin-in-r
degrees_to_radians<-function(degrees=45,minutes=30)
{
  if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
  if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
  decimal<-minutes/60
  c.num<-degrees+decimal
  radians<-c.num*pi/180
  radians
}


# refined fa.parallel function: added xlabel option
fa.parallel.refined <- function (x, n.obs = NULL, fm = "minres", fa = "both", main = "Parallel Analysis Scree Plots", 
                                 n.iter = 20, error.bars = FALSE, se.bars = FALSE, SMC = FALSE, 
                                 ylabel = NULL, xlabel = NULL, show.legend = TRUE, sim = TRUE, quant = 0.95, 
                                 cor = "cor", use = "pairwise", plot = TRUE, correct = 0.5) 
{
  cl <- match.call()
  ci <- 1.96
  arrow.len <- 0.05
  nsub <- dim(x)[1]
  nvariables <- dim(x)[2]
  resample <- TRUE
  if ((isCorrelation(x)) && !sim) {
    warning("You specified a correlation matrix, but asked to just resample (sim was set to FALSE).  This is impossible, so sim is set to TRUE")
    sim <- TRUE
    resample <- FALSE
  }
  if (!is.null(n.obs)) {
    nsub <- n.obs
    rx <- x
    resample <- FALSE
    if (dim(x)[1] != dim(x)[2]) {
      warning("You specified the number of subjects, implying a correlation matrix, but do not have a correlation matrix, correlations found ")
      switch(cor, cor = {
        rx <- cor(x, use = use)
      }, cov = {
        rx <- cov(x, use = use)
        covar <- TRUE
      }, tet = {
        rx <- tetrachoric(x, correct = correct)$rho
      }, poly = {
        rx <- polychoric(x, correct = correct)$rho
      }, mixed = {
        rx <- mixedCor(x, use = use, correct = correct)$rho
      }, Yuleb = {
        rx <- YuleCor(x, , bonett = TRUE)$rho
      }, YuleQ = {
        rx <- YuleCor(x, 1)$rho
      }, YuleY = {
        rx <- YuleCor(x, 0.5)$rho
      })
      if (!sim) {
        warning("You specified a correlation matrix, but asked to just resample (sim was set to FALSE).  This is impossible, so sim is set to TRUE")
        sim <- TRUE
        resample <- FALSE
      }
    }
  }
  else {
    if (isCorrelation(x)) {
      warning("It seems as if you are using a correlation matrix, but have not specified the number of cases. The number of subjects is arbitrarily set to be 100  ")
      rx <- x
      nsub = 100
      n.obs = 100
      resample <- FALSE
    }
    else {
      switch(cor, cor = {
        rx <- cor(x, use = use)
      }, cov = {
        rx <- cov(x, use = use)
        covar <- TRUE
      }, tet = {
        rx <- tetrachoric(x, correct = correct)$rho
      }, poly = {
        rx <- polychoric(x, correct = correct)$rho
      }, mixed = {
        rx <- mixedCor(x, use = use, correct = correct)$rho
      }, Yuleb = {
        rx <- YuleCor(x, , bonett = TRUE)$rho
      }, YuleQ = {
        rx <- YuleCor(x, 1)$rho
      }, YuleY = {
        rx <- YuleCor(x, 0.5)$rho
      })
    }
  }
  valuesx <- eigen(rx)$values
  if (SMC) {
    diag(rx) <- smc(rx)
    fa.valuesx <- eigen(rx)$values
  }
  else {
    fa.valuesx <- fa(rx, fm = fm, warnings = FALSE)$values
  }
  temp <- list(samp = vector("list", n.iter), samp.fa = vector("list", 
                                                               n.iter), sim = vector("list", n.iter), sim.fa = vector("list", 
                                                                                                                      n.iter))
  templist <- mclapply(1:n.iter, function(XX) {
    if (is.null(n.obs)) {
      bad <- TRUE
      while (bad) {
        sampledata <- matrix(apply(x, 2, function(y) sample(y, 
                                                            nsub, replace = TRUE)), ncol = nvariables)
        colnames(sampledata) <- colnames(x)
        switch(cor, cor = {
          C <- cor(sampledata, use = use)
        }, cov = {
          C <- cov(sampledata, use = use)
          covar <- TRUE
        }, tet = {
          C <- tetrachoric(sampledata, correct = correct)$rho
        }, poly = {
          C <- polychoric(sampledata, correct = correct)$rho
        }, mixed = {
          C <- mixedCor(sampledata, use = use, correct = correct)$rho
        }, Yuleb = {
          C <- YuleCor(sampledata, , bonett = TRUE)$rho
        }, YuleQ = {
          C <- YuleCor(sampledata, 1)$rho
        }, YuleY = {
          C <- YuleCor(sampledata, 0.5)$rho
        })
        bad <- any(is.na(C))
      }
      values.samp <- eigen(C)$values
      temp[["samp"]] <- values.samp
      if (fa != "pc") {
        if (SMC) {
          sampler <- C
          diag(sampler) <- smc(sampler)
          temp[["samp.fa"]] <- eigen(sampler)$values
        }
        else {
          temp[["samp.fa"]] <- fa(C, fm = fm, SMC = FALSE, 
                                  warnings = FALSE)$values
        }
      }
    }
    if (sim) {
      simdata = matrix(rnorm(nsub * nvariables), nrow = nsub, 
                       ncol = nvariables)
      sim.cor <- cor(simdata)
      temp[["sim"]] <- eigen(sim.cor)$values
      if (fa != "pc") {
        if (SMC) {
          diag(sim.cor) <- smc(sim.cor)
          temp[["sim.fa"]] <- eigen(sim.cor)$values
        }
        else {
          fa.values.sim <- fa(sim.cor, fm = fm, SMC = FALSE, 
                              warnings = FALSE)$values
          temp[["sim.fa"]] <- fa.values.sim
        }
      }
    }
    replicates <- list(samp = temp[["samp"]], samp.fa = temp[["samp.fa"]], 
                       sim = temp[["sim"]], sim.fa = temp[["sim.fa"]])
  })
  if (is.null(ylabel)) {
    ylabel <- switch(fa, pc = "eigen values of principal components", 
                     fa = "eigen values of principal factors", both = "eigenvalues of principal components and factor analysis")
  }
  values <- t(matrix(unlist(templist), ncol = n.iter))
  values.sim.mean = colMeans(values, na.rm = TRUE)
  if (!missing(quant)) {
    values.ci = apply(values, 2, function(x) quantile(x, 
                                                      quant))
  }
  else {
    values.ci <- values.sim.mean
  }
  if (se.bars) {
    values.sim.se <- apply(values, 2, sd, na.rm = TRUE)/sqrt(n.iter)
  }
  else {
    values.sim.se <- apply(values, 2, sd, na.rm = TRUE)
  }
  ymin <- min(valuesx, values.sim.mean)
  ymax <- max(valuesx, values.sim.mean)
  sim.pcr <- sim.far <- NA
  switch(fa, pc = {
    if (plot) {
      plot(valuesx, type = "b", main = main, ylab = ylabel, 
           ylim = c(ymin, ymax), xlab = "Component Number", 
           pch = 19, col = "black")
    }
    if (resample) {
      sim.pcr <- values.sim.mean[1:nvariables]
      sim.pcr.ci <- values.ci[1:nvariables]
      sim.se.pcr <- values.sim.se[1:nvariables]
      if (plot) {
        points(sim.pcr, type = "l", lty = "dashed", pch = 19, 
               col = "black")
      }
    } else {
      sim.pcr <- NA
      sim.se.pc <- NA
    }
    if (sim) {
      if (resample) {
        sim.pc <- values.sim.mean[(nvariables + 1):(2 * 
                                                      nvariables)]
        sim.pc.ci <- values.ci[(nvariables + 1):(2 * 
                                                   nvariables)]
        sim.se.pc <- values.sim.se[(nvariables + 1):(2 * 
                                                       nvariables)]
      } else {
        sim.pc <- values.sim.mean[1:nvariables]
        sim.pc.ci <- values.ci[1:nvariables]
        sim.se.pc <- values.sim.se[1:nvariables]
      }
      if (plot) {
        points(sim.pc, type = "l", lty = "dotted", pch = 19, 
               col = "black")
      }
      pc.test <- which(!(valuesx > sim.pc.ci))[1] - 1
    } else {
      sim.pc <- NA
      sim.pc.ci <- NA
      sim.se.pc <- NA
      pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 1
    }
    fa.test <- NA
    sim.far <- NA
    sim.fa <- NA
  }, fa = {
    if (plot) {
      plot(fa.valuesx, type = "b", main = main, ylab = ylabel, 
           ylim = c(ymin, ymax), xlab = xlabel, 
           pch = 19, col = "black")
    }
    sim.se.pc <- NA
    if (resample) {
      sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                                                     nvariables)]
      sim.far.ci <- values.ci[(nvariables + 1):(2 * nvariables)]
      sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                                                      nvariables)]
      if (plot) {
        points(sim.far, type = "l", lty = "dashed", pch = 19, 
               col = "black")
      }
    }
    if (sim) {
      if (resample) {
        sim.fa <- values.sim.mean[(3 * nvariables + 1):(4 * 
                                                          nvariables)]
        sim.fa.ci <- values.ci[(3 * nvariables + 1):(4 * 
                                                       nvariables)]
        sim.se.fa <- values.sim.se[(3 * nvariables + 
                                      1):(4 * nvariables)]
      } else {
        sim.fa <- values.sim.mean[(nvariables + 1):(2 * 
                                                      nvariables)]
        sim.fa.ci <- values.sim.mean[(nvariables + 1):(2 * 
                                                         nvariables)]
        sim.se.fa <- values.sim.se[(nvariables + 1):(2 * 
                                                       nvariables)]
        sim.far <- NA
        sim.far.ci <- NA
        sim.se.far <- NA
      }
      if (plot) {
        points(sim.fa, type = "l", lty = "dotted", pch = 19, 
               col = "black")
      }
      fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
        1
    } else {
      sim.fa <- NA
      fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
        1
    }
    sim.pc <- NA
    sim.pcr <- NA
    sim.se.pc <- NA
    pc.test <- NA
  }, both = {
    if (plot) {
      plot(valuesx, type = "b", main = main, ylab = ylabel, 
           ylim = c(ymin, ymax), xlab = "Factor/Component Number", 
           pch = 19, col = "black")
      points(fa.valuesx, type = "b", pch = 19, col = "black")
    }
    if (sim) {
      if (resample) {
        sim.pcr <- values.sim.mean[1:nvariables]
        sim.pcr.ci <- values.ci[1:nvariables]
        sim.se.pcr <- values.sim.se[1:nvariables]
        sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                                                       nvariables)]
        sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                                                        nvariables)]
        sim.far.ci <- values.ci[(nvariables + 1):(2 * 
                                                    nvariables)]
        sim.pc <- values.sim.mean[(2 * nvariables + 1):(3 * 
                                                          nvariables)]
        sim.pc.ci <- values.ci[(2 * nvariables + 1):(3 * 
                                                       nvariables)]
        sim.se.pc <- values.sim.se[(2 * nvariables + 
                                      1):(3 * nvariables)]
        sim.fa <- values.sim.mean[(3 * nvariables + 1):(4 * 
                                                          nvariables)]
        sim.fa.ci <- values.ci[(3 * nvariables + 1):(4 * 
                                                       nvariables)]
        sim.se.fa <- values.sim.se[(3 * nvariables + 
                                      1):(4 * nvariables)]
        pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 
          1
        fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
          1
      } else {
        sim.pc <- values.sim.mean[1:nvariables]
        sim.pc.ci <- values.ci[1:nvariables]
        sim.se.pc <- values.sim.se[1:nvariables]
        sim.fa <- values.sim.mean[(nvariables + 1):(2 * 
                                                      nvariables)]
        sim.fa.ci <- values.ci[(nvariables + 1):(2 * 
                                                   nvariables)]
        sim.se.fa <- values.sim.se[(nvariables + 1):(2 * 
                                                       nvariables)]
        pc.test <- which(!(valuesx > sim.pc.ci))[1] - 
          1
        fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
          1
      }
      if (plot) {
        points(sim.pc, type = "l", lty = "dotted", pch = 19, 
               col = "black")
        points(sim.fa, type = "l", lty = "dotted", pch = 19, 
               col = "black")
        points(sim.pcr, type = "l", lty = "dashed", pch = 19, 
               col = "black")
        points(sim.far, type = "l", lty = "dashed", pch = 19, 
               col = "black")
      }
      pc.test <- which(!(valuesx > sim.pc.ci))[1] - 1
      fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
        1
    } else {
      sim.pcr <- values.sim.mean[1:nvariables]
      sim.pcr.ci <- values.ci[1:nvariables]
      sim.se.pcr <- values.sim.se[1:nvariables]
      sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                                                     nvariables)]
      sim.far.ci <- values.ci[(nvariables + 1):(2 * nvariables)]
      sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                                                      nvariables)]
      sim.fa <- NA
      sim.pc <- NA
      sim.se.fa <- NA
      sim.se.pc <- NA
      pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 1
      fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
        1
    }
    if (resample) {
      if (plot) {
        points(sim.pcr, type = "l", lty = "dashed", pch = 19, 
               col = "black")
        points(sim.far, type = "l", lty = "dashed", pch = 19, 
               col = "black")
      }
    }
  })
  if (error.bars) {
    if (!any(is.na(sim.pc))) {
      for (i in 1:length(sim.pc)) {
        ycen <- sim.pc[i]
        yse <- sim.se.pc[i]
        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
               lty = NULL, lwd = par("lwd"), xpd = NULL)
      }
    }
    if (!any(is.na(sim.pcr))) {
      for (i in 1:length(sim.pcr)) {
        ycen <- sim.pcr[i]
        yse <- sim.se.pcr[i]
        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
               lty = NULL, lwd = par("lwd"), xpd = NULL)
      }
    }
    if (!any(is.na(sim.fa))) {
      for (i in 1:length(sim.fa)) {
        ycen <- sim.fa[i]
        yse <- sim.se.fa[i]
        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
               lty = NULL, lwd = par("lwd"), xpd = NULL)
      }
    }
    if (!any(is.na(sim.far))) {
      for (i in 1:length(sim.far)) {
        ycen <- sim.far[i]
        yse <- sim.se.far[i]
        arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
               length = arrow.len, angle = 90, code = 3, col = par("fg"), 
               lty = NULL, lwd = par("lwd"), xpd = NULL)
      }
    }
  }
  if (show.legend && plot) {
    if (is.null(n.obs)) {
      switch(fa, both = {
        if (sim) {
          legend("topright", c("  PC  Actual Data", "  PC  Simulated Data", 
                               " PC  Resampled Data", "  FA  Actual Data", 
                               "  FA  Simulated Data", " FA  Resampled Data"), 
                 col = c("black", "black", "black", "black", "black", 
                         "black"), pch = c(4, NA, NA, 2, NA, NA), 
                 text.col = "green4", lty = c("solid", "dotted", 
                                              "dashed", "solid", "dotted", "dashed"), 
                 merge = TRUE, bg = "gray90")
        } else {
          legend("topright", c("  PC  Actual Data", " PC  Resampled Data", 
                               "  FA  Actual Data", " FA  Resampled Data"), 
                 col = c("black", "black", "black", "black"), pch = c(4, 
                                                                NA, 2, NA, NA), text.col = "green4", lty = c("solid", 
                                                                                                             "dashed", "solid", "dashed"), merge = TRUE, 
                 bg = "gray90")
        }
      }, pc = {
        if (sim) {
          legend("topright", c("  PC  Actual Data", "  PC  Simulated Data", 
                               " PC  Resampled Data"), col = c("black", "black", 
                                                               "black", "black", "black", "black"), pch = c(4, 
                                                                                                     NA, NA, 2, NA, NA), text.col = "green4", 
                 lty = c("solid", "dotted", "dashed", "solid", 
                         "dotted", "dashed"), merge = TRUE, bg = "gray90")
        } else {
          legend("topright", c("  PC  Actual Data", " PC  Resampled Data"), 
                 col = c("black", "black", "black", "black", "black", 
                         "black"), pch = c(4, NA, NA, 2, NA, NA), 
                 text.col = "green4", lty = c("solid", "dashed", 
                                              "solid", "dotted", "dashed"), merge = TRUE, 
                 bg = "gray90")
        }
      }, fa = {
        if (sim) {
          legend("topright", c("  FA  Actual Data", "  FA  Simulated Data", 
                               " FA  Resampled Data"), col = c("black", "black", 
                                                               "black", "black", "black", "black"), pch = c(4, 
                                                                                                     NA, NA, 2, NA, NA), text.col = "green4", 
                 lty = c("solid", "dotted", "dashed", "solid", 
                         "dotted", "dashed"), merge = TRUE, bg = "gray90")
        } else {
          legend("topright", c("  FA  Actual Data", " FA  Resampled Data"), 
                 col = c("black", "black", "black", "black", "black", 
                         "black"), pch = c(4, NA, NA, 2, NA, NA), 
                 text.col = "green4", lty = c("solid", "dashed", 
                                              "solid", "dotted", "dashed"), merge = TRUE, 
                 bg = "gray90")
        }
      })
    }
    else {
      switch(fa, both = {
        legend("topright", c("PC  Actual Data", " PC  Simulated Data", 
                             "FA  Actual Data", " FA  Simulated Data"), 
               col = c("black", "black", "black", "black"), pch = c(4, 
                                                              NA, 2, NA), text.col = "green4", lty = c("solid", 
                                                                                                       "dotted", "solid", "dotted"), merge = TRUE, 
               bg = "gray90")
      }, pc = {
        legend("topright", c("PC  Actual Data", " PC  Simulated Data"), 
               col = c("black", "black", "black", "black"), pch = c(4, 
                                                              NA, 2, NA), text.col = "green4", lty = c("solid", 
                                                                                                       "dotted", "solid", "dotted"), merge = TRUE, 
               bg = "gray90")
      }, fa = {
        legend("topright", c("FA  Actual Data", " FA  Simulated Data"), 
               col = c("black", "black", "black", "black"), pch = c(4, 
                                                              NA, 2, NA), text.col = "green4", lty = c("solid", 
                                                                                                       "dotted", "solid", "dotted"), merge = TRUE, 
               bg = "gray90")
      })
    }
  }
  colnames(values) <- paste0("Sim", 1:ncol(values))
  if (fa != "pc" && plot) 
    abline(h = 1)
  results <- list(fa.values = fa.valuesx, pc.values = valuesx, 
                  pc.sim = sim.pc, pc.simr = sim.pcr, fa.sim = sim.fa, 
                  fa.simr = sim.far, nfact = fa.test, ncomp = pc.test, 
                  Call = cl)
  if (fa == "pc") {
    colnames(values)[1:nvariables] <- paste0("C", 1:nvariables)
  }
  else {
    colnames(values)[1:(2 * nvariables)] <- c(paste0("C", 
                                                     1:nvariables), paste0("F", 1:nvariables))
    if (sim) {
      if (resample) 
        colnames(values)[(2 * nvariables + 1):ncol(values)] <- c(paste0("CSim", 
                                                                        1:nvariables), paste0("Fsim", 1:nvariables))
    }
    results$nfact <- fa.test
  }
  results$ncomp <- pc.test
  results$values <- values
  cat("Parallel analysis suggests that ")
  cat("the number of factors = ", fa.test, " and the number of components = ", 
      pc.test, "\\n")
  class(results) <- c("psych", "parallel")
  return(invisible(results))
}