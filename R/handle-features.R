#' Get ensemble features
#'
#' Calculate the max, minimum, median and spread for a given score
#'
#' @param df dataframe. The features dataset to calculate from and append to
#' @param colname string. The name of the column corresponding to the feature
#' for which ensemble features will be calculated
#' @param prefix string. The prefix used when naming the ensemble feature
#' columns (defaults to the name of the column)
#' @param rev boolean. Flag to indicate that smaller scores are better
#' for this feature
#' @return Returns a dataframe that includes the new ensemble features
#' @examples
#' get_ensemble(features, "ProQ2D") %>%
#' get_ensemble(., "SAINT2", "S2", rev=TRUE)
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate summarise summarise_at rename_at
#' @export
get_ensemble <-function(df, colname, prefix=colname, rev=FALSE){
  ## for these scores lower values are better
  if (rev){
    ensemble <- df %>%
      group_by(.data$Target, .data$Set) %>%
      summarise_at(vars(colname), list(Med=median,Max=min, Min=max)) %>%
      mutate(!!paste0(prefix,"_Spr") := Max-Med) %>%
      rename_at(vars(Med, Max, Min), function(x) paste0(prefix,"_",x))
  }else{
    ## for these scores highest values are better
    ensemble <- df %>%
      group_by(.data$Target, .data$Set) %>%
      summarise_at(vars(colname), list(Med=median,Max=max, Min=min)) %>%
      mutate(!!paste0(prefix,"_Spr") := Max-Med) %>%
      rename_at(vars(Med, Max, Min), function(x) paste0(prefix,"_",x))
  }
  if (colname == "MapAlign"){
    df <- df %>% mutate(MA_LEN = max(MapLength))
  }
  return(df %>% inner_join(ensemble, by=c("Target","Set")))
}

#' Get the features
#'
#' Get the features from the input data
#'
#' @param df dataframe. Data frame containing the raw input data
#' @param noglobal boolean. Indicate whether to leave out global features
#' @param scaffold boolean. Indicate whether to use ScafFold features.
#' Defaults to FALSE, use standard RFQAmodel features
#' @return Returns a dataframe containing the features for RFQAmodel
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select group_by mutate
#' @importFrom rlang .data
#' @export
get_features <- function(df, scaffold = FALSE, noglobal=FALSE){
  ## Standard RFQAmodel as used in the paper
  if ( ! scaffold){
    df2 <- df %>%
      group_by(.data$Target, .data$Set) %>%
      select(-.data$SCOP_Class) %>%
      RFQAmodelr::get_ensemble(., "SAINT2", "S2", rev=TRUE) %>%
      RFQAmodelr::get_ensemble(., "Contact", "Con", rev=TRUE) %>%
      RFQAmodelr::get_ensemble(., "PCons", "PC") %>%
      RFQAmodelr::get_ensemble(., "ProQ2D") %>%
      RFQAmodelr::get_ensemble(., "ProQRosCenD","RosCen") %>%
      RFQAmodelr::get_ensemble(., "ProQRosFAD","RosFA") %>%
      RFQAmodelr::get_ensemble(., "ProQ3D") %>%
      RFQAmodelr::get_ensemble(., "PCombC") %>%
      RFQAmodelr::get_ensemble(., "EigenTHREADER","ET") %>%
      RFQAmodelr::get_ensemble(., "MapAlign","MA") %>%
      mutate(PPV_Max = max(PPV),
             PPV_Num = (PPV * NumCon))
  }else{
    if (! noglobal){
      df2 <- df %>%
        select(-.data$SCOP_Class) %>%
        select(-.data$PCombC) %>%
        group_by(.data$Target, .data$Set) %>%
        mutate(sPCombC = ((0.3*.data$sPCons) + (0.6*.data$sProQ3D) + .data$PPV)/1.9) %>%
        RFQAmodelr::get_ensemble(., "SAINT2", "S2", rev=TRUE) %>%
        RFQAmodelr::get_ensemble(., "Contact", "Con", rev=TRUE) %>%
        RFQAmodelr::get_ensemble(., "PCons", "PC") %>%
        RFQAmodelr::get_ensemble(., "ProQ2D") %>%
        RFQAmodelr::get_ensemble(., "ProQRosCenD","RosCen") %>%
        RFQAmodelr::get_ensemble(., "ProQRosFAD","RosFA") %>%
        RFQAmodelr::get_ensemble(., "ProQ3D") %>%
        RFQAmodelr::get_ensemble(., "sPCombC") %>%
        RFQAmodelr::get_ensemble(., "EigenTHREADER","ET") %>%
        mutate(PPV_Max = max(PPV),
               PPV_Num = (PPV * NumCon)) %>%
        RFQAmodelr::get_ensemble(., "globalfRMSD") %>%
        RFQAmodelr::get_ensemble(., "localfRMSD") %>%
        RFQAmodelr::get_ensemble(., "sPCons") %>%
        RFQAmodelr::get_ensemble(., "sProQ3D")
    }
    else{
      df2 <- df %>%
        select(-.data$SCOP_Class) %>%
        select(-.data$PCons,
               -.data$ProQ2D,
               -.data$ProQRosCenD,
               -.data$ProQRosFAD,
               -.data$ProQ3D,
               -.data$PCombC) %>%
        group_by(Target, Set) %>%
        mutate(sPCombC = ((0.3*.data$sPCons) + (0.6*.data$sProQ3D) + .data$PPV)/1.9) %>%
        RFQAmodelr::get_ensemble(., "SAINT2", "S2", rev=TRUE) %>%
        RFQAmodelr::get_ensemble(., "Contact", "Con", rev=TRUE) %>%
        #         RFQAmodelr::get_ensemble(., "PCons", "PC") %>%
        #          RFQAmodelr::get_ensemble(., "ProQ2D") %>%
        #          RFQAmodelr::get_ensemble(., "ProQRosCenD","RosCen") %>%
        #          RFQAmodelr::get_ensemble(., "ProQRosFAD","RosFA") %>%
        #          RFQAmodelr::get_ensemble(., "ProQ3D") %>%
        RFQAmodelr::get_ensemble(., "sPCombC") %>%
        RFQAmodelr::get_ensemble(., "EigenTHREADER","ET") %>%
        mutate(PPV_Max=max(PPV), PPV_Num=(PPV*NumCon)) %>%
        RFQAmodelr::get_ensemble(., "globalfRMSD") %>%
        RFQAmodelr::get_ensemble(., "localfRMSD") %>%
        RFQAmodelr::get_ensemble(., "sPCons") %>%
        RFQAmodelr::get_ensemble(., "sProQ3D")
    }
  }
  return(df2)
}

#' Get labels
#'
#' Label individual models as correct or incorrect
#'
#' @param truth vector. Model quality compared to native (e.g. TMScore) for all
#' models
#' @param cutoff float. The cutoff that defines a correct model (default 0.5)
#' @return Returns a vector of the length of the number of models, with 1 or 0
#' indicating correct or incorrect
#' @export
get_labels <- function(truth = TMScore, cutoff=0.5, rev=FALSE){
  # to be labelled as correct, the truth for a model should be
  # greater than the cutoff (i.e. TMScore)
  # unless the rev flag is TRUE, in which case it should be
  # smaller than the cutoff (i.e. RMSD)
  labels <- ifelse( (-1)^(rev)*truth >= (-1)^(rev)*cutoff , 1, 0 )
  return(labels)
}
