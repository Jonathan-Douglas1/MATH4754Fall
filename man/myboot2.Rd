% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Title}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{- number of runs}

\item{x}{- sample to create bootstraps}

\item{fun}{- the statistic to plot}

\item{alpha}{-  the condifence intercal}

\item{cx}{- cx}

\item{...}{- more args}
}
\description{
Title
}
\examples{
myboot2(iter=10000, rnorm(50, mean = 2, sd = 3),fun="mean",alpha=0.05,cx=1.5)
}
