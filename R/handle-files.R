#' Gather files
#'
#' Collect the features produced by the get_features.sh script
#'
#' @param extension string. File extension
#' @param colnames vector. Column names
#' @return Returns a data frame containing data from the specified files for
#' all the targets in the current directory
#' @importFrom magrittr "%>%"
#' @export
gather_files <- function(extension, colnames, directory="."){
  filenames <- list.files(path = directory,
                         pattern = paste0("\\.{1}",extension,"$"),
                         full.names=TRUE)
  if (length(filenames) > 0) {
   df <- lapply(filenames,
                FUN=read.table, col.names=colnames, stringsAsFactors=FALSE, fill=TRUE) %>%
     bind_rows()
   return(df)
  }
}

#' Collect files
#'
#' Read in all the specified files containing raw feature information
#' @export
collect_files <- function(directory="."){
  df <- list(RFQAmodelr::gather_files("eigen", c("Decoy","EigenTHREADER"),directory),
                #     RFQAmodelr::gather_files("mapalign", c("Decoy","MapAlign","MapLength"), directory),
                     RFQAmodelr::gather_files("saintscores",c("Decoy","Contact","SAINT2"), directory),
                     RFQAmodelr::gather_files("tmscores",c("Decoy","TMScore"), directory),
                     RFQAmodelr::gather_files("sampledtmscores",c("Decoy","sTMScore"), directory),
                     RFQAmodelr::gather_files("ppvs", c("Decoy","PPV"), directory),
                     RFQAmodelr::gather_files("localproq3dscores",c("Decoy","sProQ2D","sProQRosCenD","sProQRosFAD","sProQ3D"), directory),
                     RFQAmodelr::gather_files("proq3dscores",c("Decoy","ProQ2D","ProQRosCenD","ProQRosFAD","ProQ3D"), directory),
                     RFQAmodelr::gather_files("pcons",c("Decoy","PCons"), directory),
                     RFQAmodelr::gather_files("sampledpcons",c("Decoy","sPCons"), directory),
                     RFQAmodelr::gather_files("flexscores",c("Decoy","segmentfRMSD","globalfRMSD","localfRMSD"), directory),
                     RFQAmodelr::gather_files("sampledscores",c("Decoy","segmentRMSD","globalRMSD","localRMSD"), directory),
                #     RFQAmodelr::gather_files("fnats",c("Decoy","Fnat"), directory),
                     RFQAmodelr::gather_files("lddts",c("Decoy","LDDT"), directory),
                     RFQAmodelr::gather_files("local_lddts",c("Decoy","sLDDT"), directory))
  return(df)
}

#' Collect input files
#'
#' Collect the raw feature information from files
#'
#' @param directory string. The directory containing files (default: working)
#' @param detailsfile string. The file containing details about the targets
#' (default: "target_details.txt")
#' @return Returns a dataframe containing the raw data for the targets.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate inner_join select
#' @importFrom rlang .data
#' @export
collect_input <- function(directory=".", detailsfile="target_details.txt", targetnamelength=6){
  t <- read.table(paste0(directory,"/",detailsfile), stringsAsFactors=FALSE, header=TRUE)
  featurefiles <- RFQAmodelr::collect_files(directory)
  df <- Reduce(function(...) merge(..., by='Decoy', all.x=TRUE), featurefiles) %>%
    mutate(PPV=replace(.data$PPV, is.na(.data$PPV), 0)) %>%
    mutate(Target = substr(.data$Decoy, 1, targetnamelength),
           PPV=.data$PPV/100,
           PCombC=((0.3*.data$PCons) + (0.6*.data$ProQ3D) + .data$PPV)/1.9) %>%
    inner_join(t, by="Target") %>%
    select(.data$Set,
           .data$Target,
           .data$Decoy,
           .data$EigenTHREADER,
           .data$Contact,
           .data$SAINT2,
           .data$PPV,
           .data$NumCon,
           .data$TMScore,
           .data$PCons,
           .data$ProQ2D,
           .data$ProQRosCenD,
           .data$ProQRosFAD,
           .data$ProQ3D,
           .data$PCombC,
           .data$Length,
           .data$Beff,
           .data$SCOP_Class,
           .data$globalfRMSD,
           .data$localfRMSD,
           .data$globalRMSD,
           .data$localRMSD,
           .data$LDDT,
           .data$sLDDT,
           .data$sPCons,
           .data$sProQ3D,
           .data$sTMScore) %>%
    na.omit() %>%
    mutate(Seg=stringr::str_extract(.data$Decoy, "Seg.{1,3}_{1}") %>%
             stringr::str_extract("[0-9]{1,}") %>% as.numeric()) %>%
    mutate(Sampled = .data$Length-.data$Seg-1)
  return(df)
}

