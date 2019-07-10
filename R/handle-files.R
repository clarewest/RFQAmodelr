#'#' Gather files
#'
#' Collect the features produced by the get_features.sh script
#'
#' @param extension string. File extension
#' @param colnames vector. Column names
#' @return Returns a data frame containing data from the specified files for
#' all the targets in the current directory
gather_files <- function(extension, colnames){
  df <- lapply(list.files(pattern = paste("^.{6}\\.{1}",extension,"$",sep="")), FUN=read.table, col.names=colnames, stringsAsFactors=FALSE, fill=TRUE) %>% bind_rows()
  return(df)
}

#' Collect files
#'
#' Read in all the specified files containing raw feature information
collect_files <- function(){
  df <- list(gather_files("eigen", c("Decoy","EigenTHREADER")),
                     #                     gather_files("mapalign", c("Decoy","MapAlign","MapLength")),
                     gather_files("saintscores",c("Decoy","Contact","SAINT2")),
                     gather_files("tmscores",c("Decoy","TMScore")),
                     gather_files("sampledtmscores",c("Decoy","sTMScore")),
                     gather_files("ppvs", c("Decoy","PPV")),
                     gather_files("localproq3dscores",c("Decoy","sProQ2D","sProQRosCenD","sProQRosFAD","sProQ3D")),                                       gather_files("proq3dscores",c("Decoy","ProQ2D","ProQRosCenD","ProQRosFAD","ProQ3D")),
                     gather_files("pcons",c("Decoy","PCons")),
                     gather_files("sampledpcons",c("Decoy","sPCons")),
                     gather_files("flexscores",c("Decoy","segmentfRMSD","globalfRMSD","localfRMSD")),
                     gather_files("sampledscores",c("Decoy","segmentRMSD","globalRMSD","localRMSD")),
                     #                     gather_files("fnats",c("Decoy","Fnat")),
                     gather_files("lddts",c("Decoy","LDDT")),
                     gather_files("local_lddts",c("Decoy","sLDDT")))
  return(df)
}

#' Collect input files
#'
#' Collect the raw feature information from files in the working directory
collect_input <- function(){
  featurefiles <- collect_files()
  df <- Reduce(function(...) merge(..., by='Decoy', all.x=TRUE), featurefiles) %>%
    mutate(PPV=replace(PPV, is.na(PPV), 0)) %>%
    mutate(Target = substr(Decoy, 1, 6),
           PPV=PPV/100,
           PCombC=((0.3*PCons) + (0.6*ProQ3D) + PPV)/1.9) %>%
    inner_join(t, by="Target") %>%
    select(Set, Target, Decoy, EigenTHREADER, Contact, SAINT2, PPV, NumCon, TMScore, PCons, ProQ2D, ProQRosCenD, ProQRosFAD, ProQ3D, PCombC, Length, Beff, SCOP_Class, globalfRMSD, localfRMSD, globalRMSD, localRMSD, LDDT, sLDDT, sPCons, sProQ3D, sTMScore) %>%
    na.omit() %>%
    mutate(Seg=str_extract(Decoy, "Seg.{1,3}_{1}") %>%
             str_extract("[0-9]{1,}") %>% as.numeric()) %>%
    mutate(Sampled = Length-Seg-1)
}

#' Get target details
#'
#' Read in the target details from a specified file
#'
#' The target details file should include the following columns:
#' Target, Beff, Length
#'
#' The header should be included.
#'
#' @param string. Name of the file containing target-specific details
#' @return Returns a dataframe with the target details
get_details <- function(){
  t <- read.table("target_details.txt", stringsAsFactors=FALSE, header=TRUE)
  return(t)
}
