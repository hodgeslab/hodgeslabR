#' Summarizes data
#'
#' Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95\%).
#' Uses ddply() from the plyr library. From "Cookbook for R."
#'
#' @param data A data frame.
#' @param measurevar The name of a column that contains the variable to be summariezed
#' @param groupvars A vector containing names of columns that contain grouping variables
#' @param na.rm A boolean that indicates whether to ignore NA's
#' @param conf.interval The percent range of the confidence interval (default is 95\%)
#' @keywords hodgeslab summarySE sem sd ci
#' @export
#' @examples
#' df <- DNase; summarySE(df, measurevar="density", groupvars=c("Run","conc"))
#' df <- DNase; summarySE(df, measurevar="density", groupvars="conc")

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, median, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     median = median(xx[[col]], na.rm=na.rm),
                     mean = mean  (xx[[col]], na.rm=na.rm),
                     sd   = sd    (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  # datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
