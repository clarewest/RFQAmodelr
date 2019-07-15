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
    results$Confidence <- results %>% RFQAmodelr::get_confidence({{name}})
  }
  return(results)
}

#' Get confidence categories
#'
#' Classify targets into confidence categories according to the RFQAmodel score
#' of the highest-ranking model
#'
#' @param predictor string. Name of the column to use as the score
#' @param confidence_cutoffs vector. Optional custom confidence cutoffs
get_confidence <- function(results, predictor = RFQAmodel, confidence_cutoffs = c(0.5, 0.3, 0.1)){
  results_confidence <- results %>%
    group_by(Target) %>%
    mutate(Confidence := ifelse(max({{predictor}}) > confidence_cutoffs[1] , "High",
                               ifelse(max({{predictor}}) > confidence_cutoffs[2], "Medium",
                                      ifelse(max({{predictor}}) > confidence_cutoffs[3], "Low","Failed"))))
  return(results_confidence)
}

#' Get results statistics
#'
#' Get a table of results for how well RFQAmodel performed for a set
#'
#'@param results dataframe
#'@param predictor string. Column to use as the predictor (default RFQAmodel)
#'@param truth string. Column to use as the truth (default TMScore)
#'@param cutoff float. Cutoff that defines a correct model (default 0.5)
#'@param rev boolean. True if lower values are better for the truth (e.g. RMSD) (default FALSE)
get_stats <- function(results, predictor = RFQAmodel, truth = TMScore, cutoff = 0.5, rev = FALSE){
  if (rev){
    results <- results %>% mutate(truth = -truth)
  }
  stats <- results %>%
    bind_rows((results %>% mutate(Confidence = "All")),
              (results %>% filter(Confidence %in% c("High","Medium")) %>% mutate(Confidence = "High.and.Medium")),
              results %>% filter(Confidence != "Failed") %>% mutate(Confidence = "Predicted.Modelling.Success")) %>%
    group_by(Confidence, Target) %>%
     mutate(Best := max({{truth}}) >= cutoff) %>%
     arrange( - {{predictor}} ) %>%
     slice(1:5) %>%
     mutate(Top5 = max( {{truth}} >= cutoff)) %>%
     slice(1) %>%
     summarise(Top5 = Top5, Top1=sum( {{truth}} >= cutoff), Max = sum(Best)) %>%
     summarise(Top5 = sum(Top5), Top1=sum(Top1), Max = sum(Max), Total = length(Confidence)) %>%
     mutate(Precision.Top5 = Top5/Total, Precision.Top1 = Top1/Total)
  return(stats)
}

