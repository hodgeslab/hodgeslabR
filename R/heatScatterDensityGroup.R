#' Discrete density group values for x and y data using code from LSD::heatscatter(), from for the Hodges lab
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
#' heatScatterDensityGroup(x,y,"oranges")

heatScatterDensityGroup <- function(x, y, nrcol = 30, grid = 100, 
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
                             daltonize = daltonize, cvd = cvd, alpha = alpha, rev = rev)
  if (greyscale) {
    colpal = LSD::convertgrey(colpal)
  }
  todiscrete = function(t, tmin, tmax, bins) {
    erg = round((t - tmin)/(tmax - tmin) * bins + 0.5)
    erg = pmin(pmax(erg, 1), bins)
    return(erg)
  }
  kde2d.adj = function(x, y, h, n = 25, lims = c(range(x), 
                                                 range(y)), only = "none") {
    nx = length(x)
    gx = seq.int(lims[1], lims[2], length.out = n)
    gy = seq.int(lims[3], lims[4], length.out = n)
    bandwidth.nrd.adj = function(x) {
      r = quantile(x, c(0.25, 0.75))
      h = (r[2] - r[1])/1.34
      return(4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5))
    }
    if (missing(h)) {
      bx = bandwidth.nrd.adj(x)
      by = bandwidth.nrd.adj(y)
      if (all(c(bx, by) == 0)) {
        h = rep(0.01, 2)
      }
      else if (any(c(bx, by) == 0)) {
        h = rep(max(bx, by), 2)
      }
      else {
        h = c(bx, by)
      }
    }
    else h = rep(h, length.out = 2)
    h = h/4
    ax = outer(gx, x, "-")/h[1]
    ay = outer(gy, y, "-")/h[2]
    norm.ax = dnorm(ax)
    norm.ay = dnorm(ay)
    if (only == "x") {
      norm.ay = rep(1, length(ay))
    }
    if (only == "y") {
      norm.ax = rep(1, length(ax))
    }
    #    z = tcrossprod(matrix(norm.ax, , nx), matrix(norm.ay, , nx))/(nx * h[1] * h[2])
    z = tcrossprod(matrix(data = norm.ax, ncol = nx), matrix(data = norm.ay, ncol = nx))/(nx * h[1] * h[2])
    list(x = gx, y = gy, z = z)
  }
  if (log == "") {
    xlog = x
    ylog = y
  }
  else if (log == "x") {
    xlog = log(x, 10)
    ylog = y
  }
  else if (log == "y") {
    xlog = x
    ylog = log(y, 10)
  }
  else if (log %in% c("xy", "yx")) {
    xlog = log(x, 10)
    ylog = log(y, 10)
  }
  d = kde2d.adj(xlog, ylog, n = grid, only = only)
  xdiscrete = todiscrete(xlog, min(xlog), max(xlog), bins = grid)
  ydiscrete = todiscrete(ylog, min(ylog), max(ylog), bins = grid)
  getfrommat = function(a) {
    d$z[a[1], a[2]]
  }
  heatvec = unlist(apply(cbind(xdiscrete, ydiscrete), 1, getfrommat))
  coldiscrete = todiscrete(heatvec, min(d$z), max(d$z), bins = nrcol)
  coldiscrete
}

