% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histof.R
\name{xmatch}
\alias{xmatch}
\title{virtual crossmatch (XM)}
\usage{
xmatch(
  dA = c("1", "2"),
  dB = c("5", "7"),
  dDR = c("1", "4"),
  df.abs = cabs,
  check.validity = TRUE
)
}
\arguments{
\item{dA}{donor's HLA-A typing}

\item{dB}{donor's HLA-B typing}

\item{dDR}{donor's HLA-DR typing}

\item{df.abs}{data frame with candidates' antibodies}

\item{check.validity}{Logical to decide whether to validate input.}
}
\value{
A dataframe with candidates' ID and xm result POS/NEG
}
\description{
returns candidates' virtual crossmatch againts donor's HLA typing
}
\examples{
xmatch(dA = c('1','2'), dB = c('5','7'), dDR = c('1','4'), df.abs = cabs, check.validity = TRUE)
}
