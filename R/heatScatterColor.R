#' Color values for x and y data using code from LSD::heatscatter(), from for the Hodges lab
#' 
#' This takes in x, y, and returns color values based on a palette defined using the heatscatter() function from the "LSD" library.
#' Useful for adapting heatscatter() like behavior to plotting in ggplot2.
#'
#' @param x Values of x
#' @param y Values of y
#' @param colpal Color palette, options from LSD::disco(). Defaults to "heat".
#' @param ... All other options the same as LSD::heatscatter()
#' @keywords hodgeslab heatscatter
#' @export
#' @examples 
#' heatScatterColor(x,y,coldiscrete,colpal="oranges")

heatScatterColor <- function(x, y, coldiscrete = NULL, nrcol = 30, grid = 100, 
                                   colpal = "heat", simulate = FALSE, daltonize = FALSE, cvd = "p", 
                                   alpha = NULL, rev = FALSE, only = "none", 
                                   nlevels = 10,
                                   greyscale = FALSE, log = "", ...) {
  # derived from heatscatterpoints() in "heatscatter" package (LSD library)
  if (!is.vector(x) | !is.vector(y)) 
    stop("First two argument must be numeric vectors!")
  if (length(x) != length(y)) 
    stop("Data vectors must be of the same length!")
  sound = which((!(is.na(x) | is.nan(x) | (x == Inf) | (x == -Inf))) & (!(is.na(y) | is.nan(y) | (y == Inf) | (y ==  -Inf))))
  if (length(sound) == 0) 
    stop("There are no valid point pairs to plot!")
  x = x[sound]
  y = y[sound]
  colpal = LSD::colorpalette(colpal, nrcol, simulate = simulate, 
                             daltonize = daltonize, cvd = cvd, alpha = alpha, rev = rev, ...)
  if (greyscale)
    colpal = LSD::convertgrey(colpal)
  
  if(length(sound) == 1)
    return(colpal[1])
  
  if(is.null(coldiscrete))
    coldiscrete = heatScatterDensityGroup(x, y, nrcol, grid,, 
                                          colpal, simulate, daltonize, cvd, 
                                          alpha, rev, only, nlevels, greyscale, log, ...)

  colpal[coldiscrete]
}
