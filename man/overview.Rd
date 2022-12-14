% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overview.R
\name{overview}
\alias{overview}
\title{Overview a data frame}
\usage{
overview(data, digits = 2, language = NULL, labels = NULL, ...)
}
\arguments{
\item{data}{a data frame.}

\item{digits}{digits, default 2.}

\item{language}{language language, typically “en”, or "zh", default "en".}

\item{labels}{a data frame contain the column of term, code, and label.}

\item{...}{unused.}
}
\value{
a data frame.
}
\description{
Overview a data frame
}
\examples{
overview(aSAH)

overview(lung)
}
