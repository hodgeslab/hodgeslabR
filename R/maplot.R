#' Plots MA plot for the Hodges lab
#' 
#' This function reads a data frame either directly from the results output of DESeq2, or by importing
#' a table saved directly from such a result data frame.
#' 
#' @param res Results data frame from DESeq2. Important fields are "id", "baseMean", "log2FoldChange", "padj", and "label".
#' @param foldthresh Threshold for calling changes in either direction based on fold change. Defaults to log2(1.5).
#' @param pthresh Threshold for calling changes in either direction based on padj. Defaults to 0.10.
#' @param colpal_unch Color palette for unchanged sites, options from LSD::disco(). Defaults to "greys".
#' @param colpal_incr Color palette for increased sites, options from LSD::disco(). Defaults to "oranges".
#' @param colpal_decr Color palette for decreased sites, options from LSD::disco(). Defaults to "blues".
#' @param minCount Minimum x value to plot. Defaults to 0.
#' @param ylim Two-element vector setting the y-axis limits. Defaults to c(-2.25,2.25).
#' @param label Variable describing how or if labeling should occur. Can be TRUE, FALSE, or "auto". Defaults to FALSE.
#' @param maxlabel Indicates maximum number of labels to place on the plot. Only is evaluated when label == TRUE. Defaults to 50.
#' @param grid Boolean describing whether to show major gridlines. Defaults to FALSE.
#' @param box Boolean describing whether to place a box around the plot area. Defaults to TRUE.
#' @keywords hodgeslab maplot
#' @export
#' @examples 
#' maplot(res)

maplot <- function (res, foldthresh=log2(1.5), pthresh=0.10,
                    colpal_unch="greys", colpal_incr="oranges", colpal_decr="blues", minCount=0,
                    ylim=c(-2.25,2.25), label=F, maxlabel=50, grid=F, box=T, ...) {
  
  res$densityGroup <- heatScatterDensityGroup(log10(res$baseMean),res$log2FoldChange)
  unch <- (is.na(res$padj) | !(res$padj<pthresh & abs(res$log2FoldChange) > foldthresh)) & res$baseMean > minCount
  incr <- (!is.na(res$padj) & res$padj<pthresh & res$log2FoldChange > foldthresh) & res$baseMean > minCount
  decr <- (!is.na(res$padj) & res$padj<pthresh & res$log2FoldChange < -foldthresh) & res$baseMean > minCount
  
  res$color <- NA
  if(sum(unch) > 0)
    res$color[unch] <- heatScatterColor(log10(res$baseMean[unch]),res$log2FoldChange[unch], res$densityGroup[unch], colpal=colpal_unch, rev=F)
  if(sum(incr) > 0)
    res$color[incr] <- heatScatterColor(log10(res$baseMean[incr]),res$log2FoldChange[incr], res$densityGroup[incr], colpal=colpal_incr, rev=F)
  if(sum(decr) > 0)
    res$color[decr] <- heatScatterColor(log10(res$baseMean[decr]),res$log2FoldChange[decr], res$densityGroup[decr], colpal=colpal_decr, rev=F)
  
  res$alpha <- 1
  #res$alpha[unch] <- 0.75
  
  p1 <- ggplot(res,aes(x=baseMean, y=log2FoldChange, color=color, alpha=alpha, label=label)) +
      theme_hodgeslab_basic(grid=grid, box=box) +
      scale_x_log10() +
      scale_y_continuous(expand = c(0,0.05), limits = ylim) +
      geom_point(stroke=0,size=0.75*linescale,aes(alpha=alpha)) + 
      xlab("number of reads (norm.)") +
      ylab("log2 fold change") +
      #geom_ribbon(aes(ymin = -foldthresh, ymax = foldthresh, fill = "white", alpha = 0.5, color=NULL)) +
      #geom_ribbon(data=data.frame(x=c(0.9*min(res$baseMean),1.1*max(res$baseMean))),aes(x=x,ymin = -foldthresh, ymax = foldthresh, fill = "white", alpha = 0.5, color=NULL), inherit.aes=F) +
#      geom_ribbon(data=data.frame(x=c(10^-0.05*min(res$baseMean),10^0.05*max(res$baseMean))),aes(x=x,ymin = -foldthresh, ymax = foldthresh, fill = "white", alpha = 0.5, color=NULL), inherit.aes=F) +

      geom_hline(yintercept = foldthresh, color="#d85218",size=0.75*linescale) +
      geom_hline(yintercept = -foldthresh, color="#0071bc",size=0.75*linescale) +
      scale_color_identity(guide=F) +
      scale_alpha_identity() +
      scale_fill_identity() +
      annotation_logticks(size=0.5*linescale, sides="b",
                          short=unit(1,"pt"),mid=unit(1.5,"pt"),long=unit(2,"pt")) +
      theme(panel.grid.minor = element_blank())
  
  if(label == "auto") {
    p1 <- p1 + geom_text(data = subset(res, densityGroup == 1),
                       size=4*linescale, hjust=0, vjust=0.5,
                       # nudge_y = ifelse(res$log2FoldChange > 0, 0.1,-0.1),
                       nudge_x = 0.05,
                       check_overlap=T)
  } else if(label == T) {
    stopifnot(sum(!is.na(res$label)) <= maxlabel)
    p1 <- p1 + ggrepel::geom_text_repel(
        size = 4*linescale,
        box.padding = unit(0.1, "lines"),
        point.padding = unit(0, "lines"),
        # label.padding = unit(0.1, "lines"),
        segment.size = 0.5*linescale,
        force=1,
        na.rm = T,
        nudge_x = 0.05,
        # nudge_y = ifelse(subset(res, densityGroup == 1)$log2FoldChange > 0, 0.2,-0.2)
      )
  }
  
  print(p1)

}
