#' Gather files
#'
#' Collect the features produced by the get_features.sh script
#'
#' @param extension string. File extension
#' @param colnames vector. Column names
#' @return Returns a data frame containing data from the specified files for
#' all the targets in the current directory
gather_files <- function(extension, colnames, directory="."){
  filenames <- list.files(path = directory,
                         pattern = paste0("\\.{1}",extension,"$"),
                         full.names=TRUE)
  if (length(filename) > 0) {
   df <- lapply(filenames,
                FUN=read.table, col.names=colnames, stringsAsFactors=FALSE, fill=TRUE) %>%
     bind_rows()
   return(df)
  }
}

#' Collect files
#'
#' Read in all the specified files containing raw feature information
collect_files <- function(directory="."){
  df <- list(gather_files("eigen", c("Decoy","EigenTHREADER"),directory),
                #     gather_files("mapalign", c("Decoy","MapAlign","MapLength"), directory),
                     gather_files("saintscores",c("Decoy","Contact","SAINT2"), directory),
                     gather_files("tmscores",c("Decoy","TMScore"), directory),
                     gather_files("sampledtmscores",c("Decoy","sTMScore"), directory),
                     gather_files("ppvs", c("Decoy","PPV"), directory),
                     gather_files("localproq3dscores",c("Decoy","sProQ2D","sProQRosCenD","sProQRosFAD","sProQ3D"), directory),
                     gather_files("proq3dscores",c("Decoy","ProQ2D","ProQRosCenD","ProQRosFAD","ProQ3D"), directory),
                     gather_files("pcons",c("Decoy","PCons"), directory),
                     gather_files("sampledpcons",c("Decoy","sPCons"), directory),
                     gather_files("flexscores",c("Decoy","segmentfRMSD","globalfRMSD","localfRMSD"), directory),
                     gather_files("sampledscores",c("Decoy","segmentRMSD","globalRMSD","localRMSD"), directory),
                #     gather_files("fnats",c("Decoy","Fnat"), directory),
                     gather_files("lddts",c("Decoy","LDDT"), directory),
                     gather_files("local_lddts",c("Decoy","sLDDT"), directory))
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
collect_input <- function(directory=".", detailsfile="target_details.txt"){
  t <- read.table(paste0(directory,"/",detailsfile), stringsAsFactors=FALSE, header=TRUE)
  featurefiles <- collect_files(directory)
  df <- Reduce(function(...) merge(..., by='Decoy', all.x=TRUE), featurefiles) %>%
    mutate(PPV=replace(PPV, is.na(PPV), 0)) %>%
    mutate(Target = substr(Decoy, 1, 6),
           PPV=PPV/100,
           PCombC=((0.3*PCons) + (0.6*ProQ3D) + PPV)/1.9) %>%
    inner_join(t, by="Target") %>%
    select(Set,
           Target,
           Decoy,
           EigenTHREADER,
           Contact,
           SAINT2,
           PPV,
           NumCon,
           TMScore,
           PCons,
           ProQ2D,
           ProQRosCenD,
           ProQRosFAD,
           ProQ3D,
           PCombC,
           Length,
           Beff,
           SCOP_Class,
           globalfRMSD,
           localfRMSD,
           globalRMSD,
           localRMSD,
           LDDT,
           sLDDT,
           sPCons,
           sProQ3D,
           sTMScore) %>%
    na.omit() %>%
    mutate(Seg=str_extract(Decoy, "Seg.{1,3}_{1}") %>%
             str_extract("[0-9]{1,}") %>% as.numeric()) %>%
    mutate(Sampled = Length-Seg-1)
  return(df)
}

