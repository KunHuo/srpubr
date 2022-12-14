% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-roc.R
\name{gg_roc}
\alias{gg_roc}
\title{Plot a ROC curve with ggplot2}
\usage{
gg_roc(
  data,
  outcome,
  exposure,
  positive = NULL,
  combine = FALSE,
  combine.only = FALSE,
  smooth = FALSE,
  smooth.args = list(),
  auc = FALSE,
  auc.ci = FALSE,
  auc.digits = 2,
  auc.ci.method = c("delong", "bootstrap"),
  show.cutoff = TRUE,
  line.size = 0.5,
  line.color = NULL,
  line.type = NULL,
  language = NULL,
  font.family = NULL,
  font.size = NULL,
  progress = "win",
  boot.n = 1000,
  seed = 1234,
  ...
)
}
\arguments{
\item{data}{a data frame.}

\item{outcome}{the outcome variable name contain in data, which can be a factor
or a numerical variable, but must be a binary variable.}

\item{exposure}{one or more exposure variable names are included in data.}

\item{positive}{in which  positive of outcome variable to make the comparison.
By default, positive is automatically defined. If outcome is a factor variable,
then positive is defined as the highest level. If outcome is a numerical
variable, then positive is defined as the largest value.}

\item{combine}{a logical, indicating whether to use multiple exposure variables
for joint prediction using logistic regression.}

\item{combine.only}{a logical, indicating whether to show only the results of
joint predictions.}

\item{smooth}{a logical, indicating whether to smooth a ROC curve.}

\item{smooth.args}{further arguments for smooth, see \link[pROC:smooth]{pROC::smooth} function.}

\item{auc}{a logical, indicating whether to show auc.}

\item{auc.ci}{a logical, indicating whether to show the CI of auc.}

\item{auc.digits}{digits for auc and its CIs, default 2.}

\item{auc.ci.method}{the method to use, either “delong” or “bootstrap”. The
first letter is sufficient. If omitted, the appropriate method is selected as
explained in details.}

\item{show.cutoff}{a logical, indicating whether to show cutoff.}

\item{line.size}{line size.}

\item{line.color}{line color.}

\item{line.type}{line type.}

\item{language}{language, typically “en”, or "zh", default "en".}

\item{font.family}{font family, default 'serif'.}

\item{font.size}{font size, default 12.}

\item{progress}{the name of progress bar to display. Typically “none”, “win”,
“tk” or “text”.}

\item{boot.n}{the number of bootstrap replicates, default 1000.}

\item{seed}{seed, default 1234.}

\item{...}{further arguments pass to \link{gg_theme_sci} function.}
}
\value{
a ggplot.
}
\description{
Plot a ROC curve with ggplot2
}
\examples{
# Basic example
gg_roc(aSAH,
       outcome  = "outcome",
       exposure = c("age", "s100b"))

# Combine with logistic regression.
gg_roc(aSAH,
       outcome  = "outcome",
       exposure = c("age", "s100b"),
       combine  = TRUE)

gg_roc(aSAH,
       outcome  = "outcome",
       exposure = c("age:ndka"),
       combine  = TRUE)
}
