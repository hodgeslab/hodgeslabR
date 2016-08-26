#' Geom for ggplot2 theme to add a box around the axes, for use in the Hodges lab
#' 
#' This function defines the box geom for plots coming from the Hodges lab.
#' 
#' @param line_size Sets the line width for lines used in plots, in units of pt. Defaults to 0.5.
#' @keywords hodgeslab box
#' @export
#' @examples
#' ggplot(df,aes(x=x,y=y)) + geom_point() + theme_hodgeslab_box() + geom_hodgeslab_box()

# define scaling factor to get appropriate line widths in PDFs
# at present, defined as converting from mm into "R line widths" (1/96 of an inch):
# (25.4 mm/72.27 bigpts)*(96 linepts/72 pts) = 96/72/ggplot2:::.pt

geom_hodgeslab_box <- function (line_size = 0.5) {
  theme(panel.border=element_rect(size = line_size*linescale, colour = "black", fill = NA))
}
