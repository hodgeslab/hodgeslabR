% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linescale.R
\docType{data}
\name{linescale}
\alias{linescale}
\title{Define the linescale constant for the Hodges lab plots}
\format{
An object of class \code{numeric} of length 1.
}
\usage{
linescale
}
\description{
This function converts units used in ggplot2 to Postscript sizes, so that lines can be specified using Postscript values.
}
\details{
Defines the scaling factor to get appropriate line widths in Postscript files from ggplot2 output.
At present, this value is defined as converting from mm into "R line widths" (1/96 of an inch):
(25.4 mm/72.27 bigpts)*(96 linepts/72 pts) = 96/72/ggplot2:::.pt
}
\examples{
hodgeslab::linescale
}
\keyword{hodgeslab}
\keyword{linescale}
