#' @title Merge coordinates data sets
#'
#' @description The mergeCOORD function reads all xlsx/csv files in a specific folder and creates a single output data file ("data_merged (current date).csv").
#' Input: xlsx files or csv files created with Ethovision: Analysis > Export > Raw data...(export settings for csv: File type: ANSI text, delimiter: ",").
#' The output file contains a data set with time, x and y coordinates, animal id, trial info and group (when available)
#'
#'  WARNING: depending on the number of xlsx/CSV files, this might take several minutes.
#'
#' @param startData row number in the xlsx/csv files where the data starts (the values, not the headers)
#' @param rowID row number in the xlsx/csv files that contains the animal's ID
#' @param rowDay row number in the xlsx/csv files that contains day information
#' @param rowTrial row number in the xlsx/csv files that contains trial information
#' @param rowGroup row number in the xlsx/csv files that contains the animal's group (optional)
#' @param filetype Filetype of data files. "xlsx" or "csv". Default: "xlsx".
#' @importFrom utils write.csv
#' @importFrom tibble add_column
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @importFrom dplyr pull
#' @export

mergeCOORD <- function(startData, rowID, rowDay, rowTrial, rowGroup = "FOO", filetype="xlsx"){
  # warning
  writeLines('Make sure your current working directory only contains raw MWM coordinates "xlsx" or "csv" files.\nPlease wait... This might take a moment...')

  # get file names
  files <- list.files(full.names=TRUE)

  # xslx or csv
  if (filetype == "xlsx") {
    # read data
    read <- suppressWarnings(suppressMessages(lapply(files, function(i){readxl::read_xlsx(path=i, col_names=FALSE, skip=startData-1, sheet=1)})))
    # read id, group, day and trial info
    id <- suppressWarnings(suppressMessages(lapply(files, function(i){readxl::read_xlsx(path=i, col_names=FALSE, sheet=1)[rowID,2]})))
    group <- suppressWarnings(suppressMessages(lapply(files, function(i){readxl::read_xlsx(path=i, col_names=FALSE, sheet=1)[rowGroup,2]})))
    day <- suppressWarnings(suppressMessages(lapply(files, function(i){readxl::read_xlsx(path=i, col_names=FALSE, sheet=1)[rowDay,2]})))
    trial <- suppressWarnings(suppressMessages(lapply(files, function(i){readxl::read_xlsx(path=i, col_names=FALSE, sheet=1)[rowTrial,2]})))
  } else if (filetype == "csv") {
    # read data
    read <- suppressWarnings(suppressMessages(lapply(files, function(i){readr::read_csv(i, col_names = FALSE, skip=startData-1)})))
    # read id, group, day and trial info
    id <- suppressWarnings(suppressMessages(lapply(files, function(i){readr::read_csv(i, col_names=FALSE)[rowID,2]})))
    group <- suppressWarnings(suppressMessages(lapply(files, function(i){readr::read_csv(i, col_names=FALSE)[rowGroup,2]})))
    day <- suppressWarnings(suppressMessages(lapply(files, function(i){readr::read_csv(i, col_names=FALSE)[rowDay,2]})))
    trial <- suppressWarnings(suppressMessages(lapply(files, function(i){readr::read_csv(i, col_names=FALSE)[rowTrial,2]})))
  } else stop('Please select folder with only "xlsx" or "csv" files')

  # change colnames
  colnames <- paste0("V", seq_len(ncol(read[[1]])))
  read <- lapply(read, setNames, colnames)

  # add id, group, day and trial info to data set
  for(i in 1:length(files)) {
    read[[i]] <- read[[i]] %>% tibble::add_column(Animal = pull(id[[i]]))
    read[[i]] <-   read[[i]] %>% tibble::add_column(Day = pull(day[[i]]))
    read[[i]] <-   read[[i]] %>% tibble::add_column(Trial = pull(trial[[i]]))
    if(rowGroup != "FOO")   read[[i]] <- read[[i]] %>% tibble::add_column(Group = pull(group[[i]])) else   read[[i]] <- read[[i]] %>% add_column(Group = "NA")
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
