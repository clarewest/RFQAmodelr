% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle-results.R
\name{classify_models}
\alias{classify_models}
\title{Classify models}
\usage{
classify_models(features, classifier, name = "RFQAmodel",
  confidence = FALSE)
}
\arguments{
\item{features}{data frame}

\item{classifier}{classifier object}

\item{name}{string. Optional name for score column}

\item{confidence}{boolean. Whether to also add confidence category}
}
\value{
Returns the data frame of features with the score from the classifier for each model
}
\description{
Apply an RFQAmodel classifier and attach the results
}
\examples{
results <- classify_models(validation_tab, RFQAmodel, "RFQAmodel", confidence = TRUE)
results2 <- classify_models(results, RFQAmodel_local, "RFQAmodel_local")
}
