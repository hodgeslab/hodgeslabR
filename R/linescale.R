#' Define the linescale constant for the Hodges lab plots
#' 
#' This function converts units used in ggplot2 to Postscript sizes, so that lines can be specified using Postscript values.
#'
#' Defines the scaling factor to get appropriate line widths in Postscript files from ggplot2 output.
#' At present, this value is defined as converting from mm into "R line widths" (1/96 of an inch):
#' (25.4 mm/72.27 bigpts)*(96 linepts/72 pts) = 96/72/ggplot2:::.pt
#' 
#' @keywords hodgeslab linescale
#' @export
#' @examples
#' hodgeslab::linescale

# define scaling factor to get appropriate line widths in Postscript files from ggplot2 output
# at present, defined as converting from mm into "R line widths" (1/96 of an inch):
# (25.4 mm/72.27 bigpts)*(96 linepts/72 pts) = 96/72/ggplot2:::.pt
linescale <- 25.4/72.27*96/72
