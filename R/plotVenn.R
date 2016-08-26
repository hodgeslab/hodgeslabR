#' Plots two-sample Euler diagram for the Hodges lab
#' 
#' This function accepts four variables from a contingency table mat = [a, b; c, d], and returns an Euler
#' diagram. For non-redundant set1 and set2 and all, the definition of this table is as follows:
#' a = length(intersect(set1,set2))
#' b = length(setdiff(set1,set2))
#' c = length(setdiff(set2,set1))
#' d = length(setdiff(all,union(set1,set2))
#' 
#' @param mat Contingency table as defined above.
#' @param name1 Label describing set1 contained by a+b
#' @param name2 Label describing set2 contained by a+c
#' @param nametot Label describing all data contained by a+b+c+d
#' @param theta Rotation (in degrees) of the two circles. Defaults to -30.
#' @param col Vector of length 3 describing the three sets: set1, set2, all.
#' @param method Method of calculating odds ratio OR and P value. Default value of "fisher" performs
#' two-sided fisher test and the MLE of the odds ratio. All other values returns one-tailed hypergeometric
#' test and the empirical odds ratio.
#' @param legend.position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @keywords hodgeslab venn diagram
#' @export
#' @examples
#' plotVenn(matrix(c(1,2,3,4), ncol=2),"Set 1","Set 2","All data")


plotVenn <- function(mat,name1 = "",name2 = "",nametot = "", theta = -30,
                              col = c("#a3cec6", "#7e74b5", "#8f9d3a"), method = "fisher", legend.position = "bottom") {
  a <- mat[1,1]
  b <- mat[1,2]
  c <- mat[2,1]
  d <- mat[2,2]
  
  # everything
  rtot <- sqrt((a+b+c+d)/pi)
  
  # set1
  r1 <- sqrt((a+b)/pi)
  
  # set2
  r2 <- sqrt((a+c)/pi)
  
  # set offset distance for appropriate overlap
  # if no overlap, make circle1 just outside circle2
  if(a == 0) {
    offset <- r1 + r2
  } else if(a > 0 && b == 0) {
    # if complete overlap, make offset = r2 - r1
    offset <- r2 - r1
  } else if(a > 0 && c == 0) {
    # complete overlap, but the other way around
    offset <- r1 - r2
  } else {
    # a > 0 but find the optimal distance separation
    incr <- 0.1
    d1 <- seq(abs(r2 - r1)+incr,(r1 + r2),incr)
    area <- r2^2 * acos((d1^2 + r2^2 - r1^2)/(2*d1*r2)) +
      r1^2 * acos((d1^2 + r1^2 - r2^2)/(2*d1*r1)) -
      1/2 * sqrt((-d1 + r2 + r1) * (d1 + r2 - r1) * (d1 - r2 + r1) * (d1 + r1 + r2))
    offset <- approx(x=area,y=d1,xout=a)$y
  }
  
  if(is.na(offset)) {
    disp('Warning: position calculation failed, setting to complete overlap.');
    offset <- r2 - r1
  }
  
  width <- max(r1,r2) + max(max(r1,r2), offset + min(r1,r2))
  
  # define rotation matrix (theta in degrees)
  rM <- matrix(c(cos(theta*pi/180), sin(theta*pi/180), -sin(theta*pi/180), cos(theta*pi/180)),nrow = 2)
  
  # offset centers along the x axis then rotate
  center1 <- rM %*% c(r1 - width/2, 0)
  center2 <- rM %*% c(r1 - width/2 + offset, 0)
  
  circle <- function(center = c(0,0), r = 1, npoints = 360){
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  # create master data frame
  dftot <- circle(c(0,0),rtot)
  dftot$name <- nametot
  df1 <- circle(center1,r1)
  df1$name <- name1
  df2 <- circle(center2,r2)
  df2$name <- name2
  df <- rbind(dftot,df1,df2)
  
  if(method == "fisher") {
    # two-sided fisher test with MLE of odds ratio
    res <- fisher.test(matrix(c(a,b,c,d),nrow=2))
    p <- res$p.value
    or <- res$estimate
  } else {
    # one-tailed hypergeometric test
    p <- phyper(a-1,m=a+b,n=c+d,k=a+c,lower.tail=FALSE)
    or <- a/b/(c/d)
  }
  
  rLabelPos <- 0.7
  # titlePos <- 0.95
  cropFactor <- 1.5
  
  labelx <- c(center1[1] + rLabelPos * r1 * cos(theta*pi/180),
              center1[1] - rLabelPos * r1 * cos(theta*pi/180),
              center2[1] + rLabelPos * r2 * cos(theta*pi/180),
              -rLabelPos * rtot * cos(theta*pi/180),
              0)
  labely <- c(center1[2] + rLabelPos * r1 * sin(theta*pi/180),
              center1[2] - rLabelPos * r1 * sin(theta*pi/180),
              center2[2] + rLabelPos * r2 * sin(theta*pi/180),
              rLabelPos * rtot * sin(theta*pi/180),
              -cropFactor * rtot * (rLabelPos+0.2))
  labeln <- c(a,b,c,d,paste0("Odds Ratio = ", sprintf("%0.2f",or), "\np = ",sprintf("%0.2e",p)))
  dflabel <- data.frame(x=labelx,y=labely,label=labeln)
  dflabel <- dflabel[!dflabel$label == 0, ]
  
  
  #pdf("vennDiagram.pdf",useDingbats=F,height=2,width=3)
  
  p1 <- ggplot(df,aes(x,y,fill=name,color=name)) +
    theme_hodgeslab_basic() +
    geom_polygon(alpha=0.5,size=2*linescale) +
    scale_x_continuous(expand=c(0.25,0)) +
    scale_y_continuous(expand=c(0.25,0)) +
    scale_color_manual(name="Legend",values=c(col[1], NA,NA)) +
    scale_fill_manual(name="Legend",values=c(NA,col[2],col[3])) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    coord_fixed() +
    geom_text(data=dflabel,aes(x,y,label=label),inherit.aes=F, size=5*linescale) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position=legend.position,
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  
  print(p1)
  #dev.off()
}

