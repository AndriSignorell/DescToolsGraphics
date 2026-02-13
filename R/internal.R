
# internal utils

.decToHex <- function (x) as.hexmode(as.numeric(x))

`%.nin%` <- function (x, table) match(x, table, nomatch = 0) == 0


.fm_num <- function(x, digits){
  
  format(
    x,
    digits = digits,
    nsmall = digits,
    scientific = FALSE,
    trim = TRUE
  )
  
}

.recycle <- function (...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}


.setDescToolsXOption <- function (...) {
  opts <- list(...)
  stopifnot(length(opts) > 0)
  names(opts) <- paste0("DescToolsX.", names(opts))
  options(opts)
}


.nDec <- function (x) {
  if (!inherits(x, "character")) 
    x <- as.character(x)
  res <- rep(0, length(x))
  x <- gsub(pattern = "[eE].+$", replacement = "", x = x)
  has_sep <- grep(gsub("1", "", format(1.1)), x, fixed = TRUE)
  res[has_sep] <- nchar(sub("^.+[.]", "", x))[has_sep]
  return(res)
}

.meanCI_raw <- function(x){
  
  x <- x[!is.na(x)]
  a <- qt(p = 0.025, df = length(x) - 1) * sd(x)/sqrt(length(x))
  
  return( c(est=mean(x), lci=mean(x) + a, uci=mean(x) - a) )
}



.midx <- function(x, incl.zero = FALSE, cumulate = FALSE){
  if(incl.zero) x <- c(0, x)
  res <- filter(x, rep(1/2,2))
  res <-  res[-length(res)]
  if(cumulate) res <- cumsum(res)
  return(res)
}




.linScale <- function (x, low = NULL, high = NULL, newlow = 0, newhigh = 1)  {
  
  x <- as.matrix(x)
  
  if(is.null(low)) {
    low <- apply(x, 2, min, na.rm=TRUE)
  } else {
    low <- rep(low, length.out=ncol(x))
  }
  if(is.null(high)) {
    high <- apply(x, 2, max, na.rm=TRUE)
  } else {
    high <- rep(high, length.out=ncol(x))
  }
  # do the recycling job
  newlow <- rep(newlow, length.out=ncol(x))
  newhigh <- rep(newhigh, length.out=ncol(x))
  
  xcntr <- (low * newhigh - high * newlow) / (newhigh - newlow)
  xscale <- (high - low) / (newhigh - newlow)
  
  return( scale(x, center = xcntr, scale = xscale))
  
}


.combPairs <- function(x, y = NULL) {
  
  # Note: 
  # we have this in DescToolsX too, but DescToolsGraphics must stand on its own
  
  # returns a data.frame with all pairwise combinations of two variables
  if( missing(y)) {  # kein y vorhanden, use x only
    data.frame( t(combn(x, 2L)), stringsAsFactors=FALSE )
    
  } else {
    # if y is defined, all.x to all.y will be returned  
    expand.grid(x, y, stringsAsFactors=FALSE )
  }
}




.recode <- function(x, ..., keep=NULL, elselevel=NA, ref= NULL, 
                   use.empty=FALSE, num=FALSE){
  
  # if x is character, turn it to factor and reconvert it when finished
  if(xchar <- is.character(x)){
    x <- factor(x)
  }
  
  # newlevels <- list(...)
  newlevels <- c(.setNamesX(keep, names=keep), list(...))
  
  if( sum(duplicated(unlist(newlevels))) > 0) stop ("newlevels contain non unique values!")
  
  # convert numeric values to according levels if all arguments are passed as numerics
  if(all(is.numeric(unlist(newlevels))))
    newlevels <- lapply(newlevels, function(i) levels(x)[i])
  
  if(is.null(elselevel)) { # leave elselevels as they are
    elselevels <- setdiff(levels(x), unlist(newlevels))
    names(elselevels) <- elselevels
    newlevels <- c(newlevels, elselevels)
    
  } else {
    if(!is.na(elselevel)){
      newlevels[[length(newlevels)+1]] <- setdiff(levels(x), unlist(newlevels))
      names(newlevels)[[length(newlevels)]] <- elselevel
    }
  }
  levels(x) <- newlevels
  if(!use.empty) x <- factor(x)  # delete potentially empty levels
  
  # handle NA levels
  if(any(i <- sapply(lapply(newlevels, is.na), any)))
    x[is.na(x)] <- names(newlevels)[i]
  
  if(!is.null(ref))
    x <- relevel(x, ref=ref)
  
  # x was character, convert to original then
  if(xchar)
    x <- as.character(x)
  
  if(num)
    x <- as.numeric(as.character(x))
  
  return(x)
  
}



.setNamesX <- function (x, ...) {
  
  # see also setNames()
  # args <- match.call(expand.dots = FALSE)$...
  args <- list(...)
  
  # the default when no information is provided
  if (is.null(names(args)))
    names(args) <- "names"
  
  names(args) <- lapply(names(args), match.arg, c("names", "rownames", "colnames", "dimnames"))
  
  if ("dimnames" %in% names(args)) {
    if(is.null(args[["dimnames"]]))
      dimnames(x) <-NULL
    else
      dimnames(x) <- args[["dimnames"]]
  }
  
  if ("rownames" %in% names(args)) {
    if(is.null(args[["rownames"]]))
      rownames(x) <- NULL
    else
      rownames(x) <- rep_len(args[["rownames"]], dim(x)[1])
  }
  
  if ("colnames" %in% names(args)) {
    if(is.null(args[["colnames"]]))
      colnames(x) <- NULL
    else
      colnames(x) <- rep_len(args[["colnames"]], dim(x)[2])
  }
  
  if ("names" %in% names(args)) {
    if(is.null(args[["names"]]))
      names(x) <-NULL
    else
      names(x) <- rep_len(args[["names"]], length(x))
  }
  
  x
  
}





.recycle <- function(...){
  lst <- list(...)
  
  # optimization suggestion by moodymudskipper 20.11.2019  
  maxdim <- max(lengths(lst)) # instead of max(unlist(lapply(lst, length)))
  # recycle all params to maxdim
  # res <- lapply(lst, rep_len, length.out=maxdim)
  
  # rep_len would not work for Dates
  res <- lapply(lst, rep, length.out=maxdim)
  
  attr(res, "maxdim") <- maxdim
  
  return(res)
}




.strTrunc <- function (x, maxlen = 20, ellipsis="...", wbound=FALSE) {
  
  # replace NAs with blanks, and store the indices
  x[!(valid <- !is.na(x))] <- ""
  
  # recycle max length
  maxlen <- rep(maxlen, length.out = length(x))
  
  # correct for word boundaries
  if (wbound) {
    for(i in seq_along(x)){
      
      # only change maxlen for overlong strings
      if(nchar(x[i]) > maxlen[i]){
        # get all word boundaries
        ll <- gregexpr("\\b\\W+\\b", x[i], perl = TRUE)[[1]]
        j <- ll <= maxlen[i]
        
        # use minimum of original maxlen and closest smaller maxlen respecting word boundaries 
        maxlen[i] <- 
          if(all(!j)) {
            # length of first word is > maxlen, so return maxlen 
            maxlen[i]     
          } else {
            max(ll[ll <= maxlen[i]])
          }
      }
    }
  }
  
  res <- paste0(substr(x, 0L, maxlen), ifelse(nchar(x) > maxlen, ellipsis, ""))
  
  # restore NAs
  res[!valid] <- NA_character_
  return(res)
  
}


.binomCI <- function(x, n, conf.level=0.95){
  .setNamesX(c(est=x/n, prop.test(x, n, conf.level = conf.level, correct=FALSE)$conf.int),
            names=c("est", "lci", "uci"))
}


# .binomCI(3,24)
# DescToolsX::binomCI(3, 24)



