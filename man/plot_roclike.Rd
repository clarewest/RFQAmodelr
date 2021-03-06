% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle-plots.R
\name{plot_roclike}
\alias{plot_roclike}
\title{Plot ROC-like Curve}
\usage{
plot_roclike(rocresults)
}
\arguments{
\item{rocresults}{The object output from \code{plot_roc()}, which includes
the ROC plot, results, list of features, and the maximum number of targets
with at least one correct model.}
}
\description{
Plot a ROC-like curve, showing True Positives vs False Positive Rate. This
is necessary when the total number of true positives differs between
methods, i.e. when looking at the highest-ranking model per target.
}
\details{
This function requires the object output from \code{plot_roc()}.
}
