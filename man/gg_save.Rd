% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-ggplot2.R
\name{gg_save}
\alias{gg_save}
\title{Save a ggplot (or other grid object) with sensible defaults}
\usage{
gg_save(
  plot,
  path,
  width = 8.3,
  height = width/8 * 7,
  units = "cm",
  language = "en",
  ...
)
}
\arguments{
\item{plot}{Plot to save, defaults to last plot displayed.}

\item{path}{Path to save plot to (combined with file name).}

\item{width}{Width, default 8.3.}

\item{height}{Height.}

\item{units}{Units, default cm.}

\item{language}{language.}

\item{...}{Other arguments passed on to the graphics device function, as specified by device.}
}
\description{
Save a ggplot (or other grid object) with sensible defaults
}
