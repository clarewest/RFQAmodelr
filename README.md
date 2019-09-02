# RFQAmodelr

<!-- badges: start -->
<!-- badges: end -->

RFQAmodelr is a package to simplify using RFQAmodel to classify predicted 
protein structures as correct or incorrect. 


## Installation

You can install the development version of RFQAmodelr from GitHub with:

``` r
devtools::install_github("clarewest/RFQAmodelr")
```

RFQAmodelr also requires randomForest:

``` r
library(randomForest)
```

## Example

This is an example using the validation set models from the RFQAmodel paper:

``` r
library(RFQAmodelr)
library(randomForest)

## Read in the input scores
input <- read.table("https://raw.githubusercontent.com/clarewest/RFQAmodel/master/data/RFQAmodel_validation.txt", header=TRUE)

## Calculate ensemble features
features <- RFQAmodelr::get_features(input)

## Load classifier
load("RFQAmodel_classifier.Rda")

## Classify models
classifications <- RFQAmodelr::classify_models(features, classifier=RFQAmodel)
## By default the new column will be named RFQAmodel
## For a different name, use optional argument name = string

## Add confidence categories
confidence <- RFQAmodelr::get_confidence(classifications)
# Optional arguments:
# predictior = "RFQAmodel"
# - column to use as the predictior
# confidence_cutoffs = c(0.5, 0.3, 0.1)
# - custom confidence cutoff levels for High, Medium and Low confidence
```

This can be done in a single step using e.g. dplyr:

```r

library(dplyr)
results <-
  RFQAmodelr::get_features(input) %>%
  RFQAmodelr::classify_models(., classifier=RFQAmodel) %>%
  RFQAmodelr::get_confidence(.)

```

