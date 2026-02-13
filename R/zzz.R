
#' @useDynLib DescToolsGraphics, .registration = TRUE

#' @importFrom Rcpp sourceCpp
#' 
#' @importFrom graphics plot hist abline par points text
#'             axTicks axis grid layout lines mtext rect title polygon
#'             strheight strwidth clip  image grconvertX grconvertY
#'             segments barplot box matplot layout.show arrows 
#'             plot.new plot.window
#'             
#' @importFrom grDevices rgb col2rgb rgb2hsv colors colorRampPalette adjustcolor
#'             xy.coords heat.colors dev.size gray.colors
#'             
#' @importFrom utils head tail combn
#' 
#' @importFrom stats qt sd as.dendrogram dist hclust order.dendrogram filter 
#'             relevel setNames is.ts time prop.test predict qnorm formula var
#'             model.frame model.response model.weights terms
#' 
NULL
