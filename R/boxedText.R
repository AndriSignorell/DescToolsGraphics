
#' Add Text in a Box to a Plot 
#' 
#' boxedText draws the strings given in the vector labels at the coordinates
#' given by x and y, surrounded by a rectangle. 
#' 
#' @name boxedText
#' @aliases boxedText boxedText.default
#' @param x,y numeric vectors of coordinates where the text labels should be
#' written. If the length of x and y differs, the shorter one is recycled. 
#' @param labels a character vector or expression specifying the text to be
#' written.  An attempt is made to coerce other language objects (names and
#' calls) to expressions, and vectors and other classed objects to character
#' vectors by as.character. If labels is longer than x and y, the coordinates
#' are recycled to the length of labels. 
#' @param adj The value of adj determines the way in which text strings are
#' justified.  A value of 0 produces left-justified text, 0.5 (the default)
#' centered text and 1 right-justified text.  (Any value in \verb{[0, 1]} is allowed,
#' and on most devices values outside that interval will also work.)  Note that
#' the adj argument of text also allows adj = c(x, y) for different adjustment
#' in x- and y- directions.
#' 
#' @param pos a position specifier for the text. If specified this overrides
#' any adj value given. Values of 1, 2, 3 and 4, respectively indicate
#' positions below, to the left of, above and to the right of the specified
#' coordinates. 
#' @param offset when pos is specified, this value gives the offset of the
#' label from the specified coordinate in fractions of a character width. 
#' @param vfont \code{NULL} for the current font family, or a character vector
#' of length 2 for Hershey vector fonts. The first element of the vector
#' selects a typeface and the second element selects a style. Ignored if labels
#' is an expression. 
#' @param cex numeric character expansion factor; multiplied by
#' \code{par("cex")} yields the final character size. \code{NULL} and \code{NA}
#' are equivalent to 1.0. 
#' @param col,font the color and (if vfont = NULL) font to be used, possibly
#' vectors. These default to the values of the global graphical parameters in
#' \code{par()}. 
#' @param srt The string rotation in degrees.  
#' @param xpad,ypad The proportion of the rectangles to the extent of the text
#' within. 
#' @param density the density of shading lines, in lines per inch. The default
#' value of \code{NULL} means that no shading lines are drawn.  A zero value of
#' density means no shading lines whereas negative values (and NA) suppress
#' shading (and so allow color filling). 
#' @param angle angle (in degrees) of the shading lines. 
#' @param bg color(s) to fill or shade the rectangle(s) with. The default
#' \code{NA} (or also NULL) means do not fill, i.e., draw transparent
#' rectangles, unless density is specified. 
#' @param border color for rectangle border(s). The default is
#' \code{par("fg")}. Use \code{border = NA} to omit borders (this is the
#' default).  If there are shading lines, \code{border = TRUE} means use the
#' same colour for the border as for the shading lines. 
#' @param lty line type for borders and shading; defaults to \code{"solid"}. 
#' @param lwd line width for borders and shading. Note that the use of
#' \code{lwd = 0} (as in the examples) is device-dependent. 
#' @param \dots additional arguments are passed to the text function. 
#' 
#' @param formula A formula of the form \code{lhs ~ rhs}, where \code{lhs}
#'   gives the response values and \code{rhs} the corresponding groups
#'   or explanatory variables.
#'
#' @param data An optional matrix or data frame (or similar; see
#'   \code{\link[stats]{model.frame}}) containing the variables in the
#'   formula. By default the variables are taken from
#'   \code{environment(formula)}.
#'
#' @param subset An optional vector specifying a subset of observations
#'   to be used in the analysis.
#'

#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{spreadOut}}, similar function in package \pkg{plotrix}
#' \code{\link[plotrix]{boxed.labels}} (lacking rotation option) 
#' @keywords aplot misc
#' @examples
#' 
#' canvas(xpd=TRUE)
#' 
#' boxedText(0, 0, adj=0, label="This is boxed text", srt=seq(0,360,20), xpad=.3, ypad=.3)
#' points(0,0, pch=15)
#' 

#' @rdname boxedText
#' @export
boxedText <- function(x, ...) 
  UseMethod("boxedText")



#' @rdname boxedText
#' @export
boxedText.default <- function(x, y = NULL, labels = seq_along(x), adj = NULL,
                              pos = NULL, offset = 0.5, vfont = NULL,
                              cex = 1, col = NULL, font = NULL, srt = 0, xpad = 0.2, ypad=0.2,
                              density = NULL, angle = 45,
                              bg = NA, border = par("fg"), lty = par("lty"), lwd = par("lwd"), ...) {
  
  
  .boxedText <- function(x, y = NULL, labels = seq_along(x), adj = NULL,
                         pos = NA, offset = 0.5, vfont = NULL,
                         cex = 1, col = NULL, font = NULL, srt = 0, xpad = 0.2, ypad=0.2,
                         density = NULL, angle = 45,
                         bg = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), ...) {
    
    # we don't manage to remove the color otherwise
    if(is.na(bg)) density <- 0
    
    if(is.na(pos)) pos <- NULL   # we have to change default NULL to NA to be able to repeat it
    if(is.na(vfont)) vfont <- NULL
    
    w <- strwidth(labels, cex=cex, font=font, vfont=vfont)
    h <- strheight(labels, cex=cex, font=font, vfont=vfont)
    
    if(length(adj) == 1) adj <- c(adj, 0.5)
    
    xl <- x - adj[1] * w - strwidth("M", cex=cex, font=font, vfont=vfont) * xpad
    xr <- xl + w + 2*strwidth("M", cex=cex, font=font, vfont=vfont) * xpad
    
    yb <- y - adj[2] * h - strheight("M", cex=cex, font=font, vfont=vfont) * ypad
    yt <- yb + h + 2*strheight("M", cex=cex, font=font, vfont=vfont) * ypad
    
    xy <- rotate(x=c(xl,xl,xr,xr), y=c(yb,yt,yt,yb), mx=x, my=y, theta=degToRad(srt))
    polygon(x=xy$x, y=xy$y, col=bg, density=density, angle=angle, border=border, lty=lty, lwd=lwd, ...)
    
    text(x=x, y=y, labels=labels, adj=adj, pos=pos, offset=offset, vfont=vfont, cex=cex, col=col, font=font, srt=srt)
  }
  
  x <- xy.coords(x, y, recycle = TRUE, setLab = FALSE)
  
  if(is.null(adj))
    adj <- c(0.5, 0.5)
  else
    adj <- rep(adj, length.out=2)
  if (is.null(pos)) pos <- NA
  if (is.null(vfont)) vfont <- NA
  if (is.null(col)) col <- par("fg")
  if (is.null(font)) font <- 1
  if (is.null(density)) density <- NA
  
  # recyle arguments:
  #   which parameter has the highest dimension
  # attention: we cannot repeat NULLs but we can repeat NAs, so we swap NULLs to NAs and
  #            reset them to NULL above
  lst <- list(x=x$x, y=x$y, labels=labels, pos=pos, offset=offset, vfont=vfont,
              cex=cex, col=col, font=font, srt=srt, xpad=xpad, ypad=ypad,
              density=density, angle=angle, bg=bg, border=border, lty=lty, lwd=lwd)
  maxdim <- max(unlist(lapply(lst, length)))
  
  # recycle all params to maxdim
  lgp <- lapply(lst, rep, length.out=maxdim )
  lgp$adj <- as.list(data.frame(replicate(adj, n=maxdim)))
  
  for( i in 1:maxdim){
    .boxedText(
      x=lgp$x[i], y=lgp$y[i], labels=lgp$labels[i], adj=lgp$adj[[i]], pos=lgp$pos[i], offset=lgp$offset[i]
      , vfont=lgp$vfont[i], cex=lgp$cex[i], col=lgp$col[i], font=lgp$font[i]
      , srt=lgp$srt[i], xpad=lgp$xpad[i], ypad=lgp$ypad[i], density=lgp$density[i]
      , angle=lgp$angle[i], bg=lgp$bg[i], border=lgp$border[i], lty=lgp$lty[i], lwd=lgp$lwd[i] )
  }
}


#' @rdname boxedText
#' @export
boxedText.formula <- function (formula, data = parent.frame(), ..., subset) {
  
  m <- match.call(expand.dots = FALSE)
  eframe <- parent.frame()
  md <- eval(m$data, eframe)
  if (is.matrix(md)) 
    m$data <- md <- as.data.frame(data)
  dots <- lapply(m$..., eval, md, eframe)
  m$... <- NULL
  m <- as.list(m)
  m[[1L]] <- stats::model.frame.default
  m <- as.call(c(m, list(na.action = NULL)))
  mf <- eval(m, eframe)
  
  if (!missing(subset)) {
    s <- eval(m$subset, data, eframe)
    if (!missing(data)) {
      l <- nrow(data)
    } else {
      mtmp <- m
      mtmp$subset <- NULL
      l <- nrow(eval(mtmp, eframe))
    }
    
    dosub <- function(x) if (length(x) == l) 
      x[s]
    else x
    
    dots <- lapply(dots, dosub)
  }
  
  response <- attr(attr(mf, "terms"), "response")
  
  if (response) {
    varnames <- names(mf)
    y <- mf[[response]]
    if (length(varnames) > 2L) 
      stop("cannot handle more than one 'x' coordinate")
    xn <- varnames[-response]
    if (length(xn) == 0L) 
      do.call("boxedText", c(list(y), dots))
    else do.call("boxedText", c(list(mf[[xn]], y), dots))
    
  } else stop("must have a response variable")
  
}

