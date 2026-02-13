
#' Cleveland's Dot Plots
#' 
#' Draw a Cleveland dot plot. This is an extended version of
#' \code{\link{dotchart}} with an added option for error bars, an \code{add}
#' argument and several more options. \code{PlotCI()} is a small helpfunction
#' to facilitate ci-plots of several models.
#' 
#' Dot plots are a reasonable substitute for bar plots. This function is
#' invoked to produce dotplots as described in Cleveland (1985).
#' 
#' For \code{plotDotCI()} the dots are a list of matrices with 3 columns,
#' whereas the first is the coefficent, the second the lower and the third the
#' upper end of the confidence interval.
#' 
#' @name plotDot
#' @aliases plotDot plotDotCI
#' @param x either a vector or matrix of numeric values (\code{NA}s are
#' allowed).  If \code{x} is a matrix the overall plot consists of juxtaposed
#' dotplots for each row.  Inputs which satisfy \code{\link{is.numeric}(x)} but
#' not \code{is.vector(x) || is.matrix(x)} are coerced by
#' \code{\link{as.numeric}}, with a warning.
#' @param labels a vector of labels for each point.  For vectors the default is
#' to use \code{names(x)} and for matrices the row labels
#' \code{dimnames(x)[[1]]}.
#' @param groups an optional factor indicating how the elements of \code{x} are
#' grouped.  If \code{x} is a matrix, \code{groups} will default to the columns
#' of \code{x}.
#' @param gdata data values for the groups.  This is typically a summary such
#' as the median or mean of each group.
#' @param cex the character size to be used.  Setting \code{cex} to a value
#' smaller than one can be a useful way of avoiding label overlap.  Unlike many
#' other graphics functions, this sets the actual size, not a multiple of
#' \code{par("cex")}.
#' @param pch the plotting character or symbol to be used. Default is 21.
#' @param gpch the plotting character or symbol to be used for group values.
#' @param bg the background color of plotting characters or symbols to be used;
#' use \code{\link{par}(bg= *)} to set the background color of the whole plot.
#' @param color the color(s) to be used for points and labels.
#' @param gcolor the single color to be used for group labels and values.
#' @param lcolor the color(s) to be used for the horizontal lines.
#' @param lblcolor the color(s) to be used for labels.
#' @param xlim horizontal range for the plot, see \code{\link{plot.window}},
#' e.g.
#' @param main overall title for the plot, see \code{\link{title}}.
#' @param xlab,ylab axis annotations as in \code{title}.
#' @param xaxt a character which specifies the x axis type. Specifying
#' \code{"n"} suppresses plotting of the axis.
#' @param yaxt a character which specifies the y axis type. Specifying
#' \code{"n"} suppresses plotting of the axis.
#' @param add logical specifying if bars should be added to an already existing
#' plot; defaults to \code{FALSE}.
#' @param args.errbars optional arguments for adding error bars. All arguments
#' for \code{\link{errBars}} can be supplied. If left to \code{NULL} (default),
#' no error bars will be plotted.
#' @param cex.axis The magnification to be used for axis annotation relative to
#' the current setting of cex.
#' @param cex.pch The magnification to be used for plot symbols relative to the
#' current setting of cex.
#' @param cex.gpch The magnification to be used for group symbols relative to
#' the current setting of cex.
#' @param gshift the number of characters, for which the grouplabels should be
#' shift to the left compared to the sublabels.
#' @param automar logical (default \code{TRUE}), defining if the left margin
#' should be set according to the width of the given labels, resp. grouplabels.
#' If set to \code{FALSE} the margins are taken from \code{par("mar")}.
#' @param \dots \link{graphical parameters} can also be specified as arguments.
#' @param grp an integer, defining if the the coefficients should be grouped
#' along the first or the second dimension (default is 1).
#' @return Return the y-values used for plotting.
#' @author R-Core with some extensions by Andri Signorell
#' <andri@@signorell.net>
#' @seealso \code{\link{dotchart}}, \code{\link{plotDotCI}}
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}.  Wadsworth & Brooks/Cole.
#' 
#' Cleveland, W. S. (1985) \emph{The Elements of Graphing Data.} Monterey, CA:
#' Wadsworth.
#' 
#' Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
#' @keywords hplot
#' @examples
#' 
#' plotDot(VADeaths, main = "Death Rates in Virginia - 1940")
#' op <- par(xaxs = "i")  # 0 -- 100%
#' plotDot(t(VADeaths), xlim = c(0,100),
#'         main = "Death Rates in Virginia - 1940")
#' par(op)
#' 
#' # add some error bars
#' plotDot(VADeaths, main="Death Rates in Virginia - 1940", col="red", pch=21,
#'         args.errbars = list(from=VADeaths-2, to=VADeaths+2, mid=VADeaths,
#'                             cex=1.4))
#' 
#' # add some other values
#' plotDot(VADeaths+3, pch=15, col="blue", add=TRUE)
#' 
#' # same as plotDotCI
#' fit <- lm(Fertility ~ ., swiss)
#' xci <- cbind(coef(fit), confint(fit))[-1, ]
#' plotDot(xci[,1], main="Fertility Fit", pch=21, bg="grey80", col="black",
#'         args.errbars = list(from=xci[,2], to=xci[,3], 
#'                  mid=xci[,1], lwd=2, col="grey40", cex=1.5))
#' 
#' plotDot(VADeaths, main="Death Rates in Virginia - 1940", pch="|", 
#'         lcolor = "navajowhite3", col="deeppink4",
#'         args.errbars = list(from=VADeaths-2, to=VADeaths+2, mid=VADeaths,
#'                             cex=1.3, lwd=8, code=0, col="green"))
#' 
#' # Setting the colours
#' # define some error bars first
#' lci <- sweep(x = VADeaths, MARGIN = 2, FUN = "-", 1:4)
#' uci <- sweep(x = VADeaths, MARGIN = 1, FUN = "+", 1:5)
#' 
#' plotDot(VADeaths, main="This should only show how to set the colours, not be pretty",
#'         pch=21, col=c("blue","grey"), bg=c("red", "yellow"),
#'         gcolor = c("green", "blue", "orange", "magenta"), gdata=c(10,20,30,40),
#'         gpch = c(15:18), lcolor = "orange",
#'         args.errbars = list(from=lci, to=uci, mid=VADeaths, cex=1.4))
#' 


#' @rdname plotDot
#' @export
plotDot <- function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"),
                     pch = 21, gpch = 21, bg = par("bg"), color = par("fg"), gcolor = par("fg"),
                     lcolor = "gray", lblcolor = par("fg"), xlim = NULL, main = NULL, xlab = NULL, ylab = NULL, xaxt=NULL, yaxt=NULL,
                     add = FALSE, args.errbars = NULL, cex.axis=par("cex.axis"), cex.pch=1.2, 
                     cex.gpch=1.2, gshift=2, automar=TRUE, ...) {
  
  ErrBarArgs <- function(from, to = NULL, pos = NULL, mid = NULL,
                         horiz = FALSE, col = par("fg"), lty = par("lty"), lwd = par("lwd"),
                         code = 3, length = 0.05, pch = NA, cex.pch = par("cex"),
                         col.pch = par("fg"), bg.pch = par("bg"), ...) {
    
    if (is.null(to)) {
      if (length(dim(x) != 1))
        stop("'to' must be be provided, if x is a matrix.")
      
      if (!dim(from)[2] %in% c(2, 3))
        stop("'from' must be a kx2 or a kx3 matrix, when 'to' is not provided.")
      if (dim(from)[2] == 2) {
        to <- from[, 2]
        from <- from[, 1]
      }
      else {
        mid <- from[, 1]
        to <- from[, 3]
        from <- from[, 2]
      }
    }
    
    if (length(dim(from)) ==2 )
      from <- from[, ncol(from):1]
    if (length(dim(to)) ==2 )
      to <- to[, ncol(to):1]
    if (length(dim(mid)) ==2 )
      mid <- mid[, ncol(mid):1]
    
    return(list(from = from, to = to, mid = mid, col = col,
                col.axis = 1, lty = lty, lwd = lwd, angle = 90, code = code,
                length = length, pch = pch, cex.pch = cex.pch, col.pch = col.pch,
                bg.pch = bg.pch))
  }
  
  # if(!is.null(args.errbars)){
  #   # switch pch and col to errorbars
  #   if(!is.null(pch)){
  #     args.errbars$pch <- pch
  #     args.errbars$col.pch <- color
  #     args.errbars$bg.pch <- bg
  #     bg <- color <- pch <- NA
  #   }
  # }
  
  x <- x[length(x):1]
  
  labels <- rev(labels)
  groups <- rev(groups)
  lcolor <- rev(lcolor)
  lblcolor <- rev(lblcolor)
  color <- rev(color)
  pch <- rev(pch)
  bg <- rev(bg)
  
  # cex <- rep(cex, length.out = 3)
  cex.axis <- rep(cex.axis, length.out = 3)
  
  if (!is.null(args.errbars))
    errb <- do.call(ErrBarArgs, args.errbars)
  if (!add && is.null(xlim)) {
    if (is.null(args.errbars)) {
      xlim <- range(x[is.finite(x)])
    }
    else {
      rng <- c(errb$from, errb$to)
      xlim <- range(pretty(rng[is.finite(rng)]))
    }
  }
  opar <- par("mai", "mar", "cex", "cex.axis", "yaxs")
  on.exit(par(opar))
  par(cex = cex, cex.axis=cex.axis[1], yaxs = "i")
  
  lheight <- strheight("M", units="inches", cex=max(cex.axis[c(2, 3)])*cex)
  
  if (!is.numeric(x))
    stop("'x' must be a numeric vector or matrix")
  n <- length(x)
  if (is.matrix(x)) {
    if (is.null(labels))
      labels <- rownames(x)
    if (is.null(labels))
      labels <- as.character(1L:nrow(x))
    labels <- rep_len(labels, n)
    if (is.null(groups))
      groups <- col(x, as.factor = TRUE)
    glabels <- levels(groups)
    
  } else {
    if (is.null(labels))
      labels <- names(x)
    glabels <- if (!is.null(groups))
      levels(groups)
    if (!is.vector(x)) {
      warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
      x <- as.numeric(x)
    }
  }
  
  if (!add)
    plot.new()
  # we must use cex*cex.axis here
  linch <- if (!is.null(labels))
    max(strwidth(labels, "inch", cex=max(cex.axis[2])* cex), na.rm = TRUE)
  else 0
  
  if (is.null(glabels)) {
    goffset <- ginch <- 0
    
  } else {
    ginch <- max(strwidth(glabels, "inch", cex=max(cex.axis[3]) * cex), na.rm = TRUE)
    goffset <- lheight  
  }
  if (!(is.null(labels) && is.null(glabels) || identical(yaxt, "n") || !automar)) {
    nmai <- par("mai")
    # nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + lheight
    # warum sollte der linke Rand so sein wie der rechte??
    nmai[2L] <- lheight + max(linch + goffset, ginch) + gshift * lheight
    par(mai = nmai)
  }
  if (is.null(groups)) {
    o <- 1L:n
    y <- o
    ylim <- c(0, n + 1)
    
  } else {
    o <- sort.list(as.numeric(groups), decreasing = TRUE)
    x <- x[o]
    groups <- groups[o]
    # color <- rep_len(color, length(groups))[o]
    # lcolor <- rep_len(lcolor, length(groups))[o]
    offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
    y <- 1L:n + 2 * offset
    ylim <- range(0, y + 2)
  }
  
  if (!add)
    plot.window(xlim = xlim, ylim = ylim, log = "")
  
  # lheight <- par("csi")
  # much more precise:
  if (!is.null(labels)) {
    linch <- max(strwidth(labels, "inch", cex = cex.axis[2])*cex, na.rm = TRUE)
    #    loffset <- (linch + 0.1)/lheight
    loffset <- grconvertX(linch + 0.1, from="inch", to="lines")
    labs <- labels[o]
    if (!identical(yaxt, "n") && !add)
      mtext(labs, side = 2, line = loffset, at = y, adj = 0,
            col = lblcolor, las = 2, cex = cex.axis[2]*cex, ...)
  }
  
  if (!add)
    abline(h = y, lty = "dotted", col = lcolor)
  
  if (!is.null(args.errbars)) {
    arrows(x0 = rev(errb$from)[o], x1 = rev(errb$to)[o],
           y0 = y, col = rev(errb$col), angle = 90, code = rev(errb$code),
           lty = rev(errb$lty), lwd = rev(errb$lwd), length = rev(errb$length))
    # if (!is.null(errb$mid))
    #   points(rev(errb$mid)[o], y = y, pch = rev(errb$pch), col = rev(errb$col.pch),
    #          cex = rev(errb$cex.pch), bg = rev(errb$bg.pch))
  }
  
  points(x, y, pch = pch, col = color, bg = bg, cex=cex * cex.pch)
  if (!is.null(groups)) {
    gpos <- rev(cumsum(rev(tapply(groups, groups, length)) +
                         2) - 1)
    
    # ginch <- max(strwidth(glabels, "inch", cex=cex.axis[3]*cex), na.rm = TRUE)
    # goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
    
    #    lgoffset <- (max(linch + goffset, ginch) + lheight)/lheight
    lgoffset <- grconvertX(max(linch + goffset, ginch) + gshift * lheight, 
                           from="inch", to="lines")
    
    if (!identical(yaxt, "n") && !add)
      mtext(glabels, side = 2, line = lgoffset, at = gpos, adj = 0,
            col = gcolor, las = 2, cex = cex.axis[3]*cex, ...)
    if (!is.null(gdata)) {
      abline(h = gpos, lty = "dotted")
      points(gdata, gpos, pch = gpch, cex=cex*cex.gpch, col = gcolor, bg = bg, ...)
    }
  }
  if (!(add || identical(xaxt, "n") ))
    axis(1)
  
  if (!add)
    box()
  
  if (!add)
    title(main = main, xlab = xlab, ylab = ylab, ...)
  
  
  if (!is.null(.getOption("stamp")) && !add)
    stamp()
  
  # invisible(y[order(o, decreasing = TRUE)])
  # replaced by 0.99.18:
  invisible(y[order(y, decreasing = TRUE)])
  
}


#' @rdname plotDot
#' @export
plotDotCI <- function(..., grp=1, cex = par("cex"),
                      pch = 21, gpch = 21, bg = par("bg"), color = par("fg"), gcolor = par("fg"),
                      lcolor = "gray", lblcolor = par("fg"), xlim = NULL, main = NULL, xlab = NULL, ylab = NULL, xaxt=NULL, yaxt=NULL,
                      cex.axis=par("cex.axis"), cex.pch=1.2,
                      cex.gpch=1.2, gshift=2, automar=TRUE){
  
  lst <- list(...)
  
  if(grp==1)
    z <- aperm(do.call(abind::abind, list(lst, along = 3)), c(1,3,2))
  else
    z <- aperm(do.call(abind::abind, list(lst, along = 3)), c(3,1,2))
  
  # ... are matrices with n rows and 3 columns, est, lci, uci
  plotDot(z[,,1],
          args.errbars = list(from=z[,,2], to=z[,,3]),
          cex = cex,
          pch = pch, gpch = gpch, bg = bg, color = color, gcolor = gcolor,
          lcolor = lcolor, lblcolor = lblcolor, xlim = xlim, main = main,
          xlab = xlab, ylab = ylab, xaxt=xaxt, yaxt=yaxt,
          cex.axis=cex.axis, cex.pch=cex.pch,
          cex.gpch=cex.gpch, gshift=gshift, automar=automar)
  
  
}

