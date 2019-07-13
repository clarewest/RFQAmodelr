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
get_ensemble <-function(df, colname, prefix=colname, rev=FALSE){
  ## for these scores lower values are better
  if (rev){
    ensemble <- df %>%
      group_by(Target, Set) %>%
      summarise_at(vars(colname), list(Med=median,Max=min, Min=max)) %>%
      mutate(!!paste0(prefix,"_Spr") := Max-Med) %>%
      rename_at(vars(Med, Max, Min), function(x) paste0(prefix,"_",x))
  }else{
    ## for these scores highest values are better
    ensemble <- df %>%
      group_by(Target, Set) %>%
      summarise_at(vars(colname), list(Med=median,Max=max, Min=min)) %>%
      mutate(!!paste0(prefix,"_Spr") := Max-Med) %>%
      rename_at(vars(Med, Max, Min), function(x) paste0(prefix,"_",x))
  }
  return(df %>% inner_join(ensemble, by=c("Target","Set")))
}

#' Get the features
#'
#' Get the features from the input data
#'
#' @param df dataframe. Data frame containing the raw input data
#' @param noglobal boolean. Indicate whether to leave out global features
#' @return Returns a dataframe containing the features for RFQAmodel
get_features <- function(df, noglobal=FALSE){
  if (! noglobal){
    df2 <- df %>%
      select(-SCOP_Class) %>%
      group_by(Target, Set) %>%
      get_ensemble(., "SAINT2", "S2", rev=TRUE) %>%
      get_ensemble(., "Contact", "Con", rev=TRUE) %>%
      get_ensemble(., "PCons", "PC") %>%
      get_ensemble(., "ProQ2D") %>%
      get_ensemble(., "ProQRosCenD","RosCen") %>%
      get_ensemble(., "ProQRosFAD","RosFA") %>%
      get_ensemble(., "ProQ3D") %>%
      get_ensemble(., "PCombC") %>%
      get_ensemble(., "EigenTHREADER","ET") %>%
      mutate(PPV_Max=max(PPV), PPV_Num=(PPV*NumCon)) %>%
      get_ensemble(., "globalfRMSD") %>%
      get_ensemble(., "localfRMSD") %>%
      get_ensemble(., "sPCons") %>%
      get_ensemble(., "sProQ3D")
  }
  else{
    df2 <- df %>%
      select(-SCOP_Class) %>%
      select(-PCons, -ProQ2D, -ProQRosCenD, -ProQRosFAD, -ProQ3D, -PCombC) %>%
      group_by(Target, Set) %>%
      mutate(sPCombC = ((0.3*sPCons) + (0.6*sProQ3D) + PPV)/1.9) %>%
      get_ensemble(., "SAINT2", "S2", rev=TRUE) %>%
      get_ensemble(., "Contact", "Con", rev=TRUE) %>%
      #         get_ensemble(., "PCons", "PC") %>%
      #          get_ensemble(., "ProQ2D") %>%
      #          get_ensemble(., "ProQRosCenD","RosCen") %>%
      #          get_ensemble(., "ProQRosFAD","RosFA") %>%
      #          get_ensemble(., "ProQ3D") %>%
      get_ensemble(., "sPCombC") %>%
      get_ensemble(., "EigenTHREADER","ET") %>%
      mutate(PPV_Max=max(PPV), PPV_Num=(PPV*NumCon)) %>%
      get_ensemble(., "globalfRMSD") %>%
      get_ensemble(., "localfRMSD") %>%
      get_ensemble(., "sPCons") %>%
      get_ensemble(., "sProQ3D")
  }
  return(df2)
}
