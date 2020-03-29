#' @title Merge coordinates data sets
#'
#' @description The mergeCOORD function reads all csv files in a specific folder and creates a single output data file ("data_merged (current date).csv").
#' Input: csv files created with Ethovision: Analysis > Export > Raw data...(export settings: File type: ANSI text, delimiter: ",").
#' The output file contains a data set with time, x and y coordinates, animal id, trial info and group (when available)
#'
#'  WARNING: depending on the number of CSV files, this might take several minutes.
#'
#' @param startData row number in the csv files where the data starts (the values, not the headers)
#' @param rowID row number in the csv files that contains the animal's ID
#' @param rowDay row number in the csv files that contains day information
#' @param rowTrial row number in the csv files that contains trial information
#' @param rowGroup row number in the csv files that contains the animal's group (optional)
#' @importFrom utils read.csv write.csv
#' @export

mergeCOORD <- function(startData, rowID, rowDay, rowTrial, rowGroup = "FOO"){
  # warning
  writeLines("Make sure your current working directory only contains raw MWM coordinates csv files.\nPlease wait... This might take a moment...")

  # get file names
  files <- list.files(full.names=TRUE)

  # read data
  read <- lapply(files, function(i){read.csv(i, header=FALSE, skip=startData-1, stringsAsFactors=FALSE)})

  # id
  id <- lapply(files, function(i){read.csv(i, header=FALSE)[rowID,2]})

  # group
  group <- lapply(files, function(i){read.csv(i, header=FALSE)[rowGroup,2]})

  # day
  day <- lapply(files, function(i){read.csv(i, header=FALSE)[rowDay,2]})

  # trial
  trial <- lapply(files, function(i){read.csv(i, header=FALSE)[rowTrial,2]})

  # add id, trial, day and group (optional) information
  for(i in 1:length(files)) {
    read[[i]]$Animal <- id[[i]]
    read[[i]]$Day <- day[[i]]
    read[[i]]$Trial <- trial[[i]]
    if(rowGroup != "FOO") read[[i]]$Group <- group[[i]] else read[[i]]$Group <- "NA"
  }

  # create dataset
  data <- do.call(rbind.data.frame, read)

  # select columns
  data <- data[,c("V2","V3","V4","Animal", "Day", "Trial", "Group")]

  # column names
  colnames(data) <- c("Time", "x", "y","Animal", "Day", "Trial", "Group")

  # transform data
  data$x <- suppressWarnings(as.numeric(as.character(data$x)))
  data$y <- suppressWarnings(as.numeric(as.character(data$y)))

  # save file
  filename <- paste("data coordinates (merged ", Sys.Date(), ").csv", sep="")
  write.csv(data, filename, row.names=FALSE)
}
