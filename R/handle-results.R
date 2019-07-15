## Note to clare: do you want these functions to return columns or data frames?
## results <- classify_models(features, RFQAmodel_classifier) %>% get_confidence(.)
## results <- features %>% mutate(RFQAmodel = classify_models(., RFQAmodel_classifier)) %>% mutate(confidence = get_confidence(RFQAmodel))
##

#' Classify models
#'
#' Apply an RFQAmodel classifier and attach the results
#'
#' @param features data frame
#' @param classifier classifier object
#' @param name string. Optional name for score column
#' @param confidence boolean. Whether to also add confidence category
#' @return Returns the data frame of features with the score from the classifier for each model
#' @examples
#' results <- classify_models(validation_tab, RFQAmodel, "RFQAmodel", confidence = TRUE)
#' results2 <- classify_models(results, RFQAmodel_local, "RFQAmodel_local")
classify_models <- function(features, classifier, name="RFQAmodel", confidence = FALSE){
  results <- features %>% ungroup() %>% mutate({{name}} := as.numeric(predict(classifier, features, type = "prob")[,2]))
  if (confidence){
    results$Confidence <- results %>% get_confidence({{name}})
  }
  return(results)
}

#' Get confidence categories
#'
#' Classify targets into confidence categories according to the RFQAmodel score
#' of the highest-ranking model
#'
#' @param score string. Name of the column to use as the score
#' @param confidence_cutoffs vector. Optional custom confidence cutoffs
get_confidence <- function(results, score = RFQAmodel, confidence_cutoffs = c(0.5, 0.3, 0.1)){
  results_confidence <- results %>%
    group_by(Target) %>%
    mutate(Confidence := ifelse(max({{score}}) > confidence_cutoffs[1] , "High",
                               ifelse(max({{score}}) > confidence_cutoffs[2], "Medium",
                                      ifelse(max({{score}}) > confidence_cutoffs[3], "Low","Failed"))))
  return(results_confidence)
}


