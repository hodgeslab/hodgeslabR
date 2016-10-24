#' Basic ggplot2 theme for the Hodges lab
#' 
#' This function defines the basic theme for plots coming from the Hodges lab.
#' 
#' @param base_size Sets the Postscript font size for all labels, in units of pt. Defaults to 7.
#' @param base_family Sets the Postscript font family for all labels. Defaults to empty "".
#' @param line_size Sets the line width for lines used in plots, in units of pt. Defaults to 0.5.
#' @keywords hodgeslab theme
#' @export
#' @examples
#' ggplot(df,aes(x=x,y=y)) + geom_point() + theme_hodgeslab_basic()

# define hodges themes
theme_hodgeslab_basic <- function(base_size = 7, base_family = "", line_size = 0.5, grid = F, rotx = 0, box = F) {

  p <- theme_classic(base_size = base_size, base_family = base_family) %+replace%
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

  if(grid == T) {
    p <- p %+replace% theme(
      panel.grid.major.x = element_line(colour = "grey80", size = 0.5*line_size*linescale),
      panel.grid.major.y = element_line(colour = "grey80", size = 0.5*line_size*linescale),
      panel.grid.minor.x = element_line(colour = "grey95", size = 0.5*line_size*linescale),
      panel.grid.minor.y = element_line(colour = "grey95", size = 0.5*line_size*linescale)
    )
  }

  if(rotx != 0) {
    p <- p %+replace% theme(
      axis.text.x = element_text(angle=rotx, size = base_size * 1, lineheight = 1, hjust = 1, vjust = 1, margin=margin(1,0,0,0))
    )
  }

  if(box == T) {
    p <- p %+replace% theme(
      axis.line.x =       element_line(size = line_size*linescale, linetype="solid", colour=NA),
      axis.line.y =       element_line(size = line_size*linescale, linetype="solid", colour=NA),
      panel.border =      element_rect(size = 2*line_size*linescale, colour = "black", fill = NA)
    )
  }

  p
}
