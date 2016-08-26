#' Grid ggplot2 theme for the Hodges lab
#' 
#' This function defines the grid theme for plots coming from the Hodges lab.
#' 
#' @param base_size Sets the Postscript font size for all labels, in units of pt. Defaults to 7.
#' @param base_family Sets the Postscript font family for all labels. Defaults to empty "".
#' @param line_size Sets the line width for lines used in plots, in units of pt. Defaults to 0.5.
#' @keywords hodgeslab theme
#' @export
#' @examples
#' ggplot(df,aes(x=x,y=y)) + geom_point() + theme_hodgeslab_grid()

theme_hodgeslab_grid <- function(base_size = 7, base_family = "", line_size = 0.5) {
  theme(
      axis.text.x =       element_text(size = base_size * 1, lineheight = 1, vjust = 1, margin=margin(1,0,0,0)),
      axis.text.y =       element_text(size = base_size * 1, lineheight = 1, hjust = 1, margin=margin(0,1,0,0)),
      axis.title.x =      element_text(size = base_size, vjust = 1, margin=margin(base_size/2,0,0,0)),
      axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0, margin=margin(0,base_size/2,0,0)),
      axis.line.x =       element_line(size = line_size*linescale, linetype="solid", colour="black"),
      axis.line.y =       element_line(size = line_size*linescale, linetype="solid", colour="black"),
      axis.ticks =        element_line(size = line_size*linescale, colour = "black"),
      legend.text =       element_text(size = base_size * 1),
      legend.key.size =   unit(0.8, "line")
  )
}
