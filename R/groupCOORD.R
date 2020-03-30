#' @title Group coordinates data
#'
#' @description The groupCOORD function uses dplyr to group data over groups, trials and/or days. Note that data will always be
#' grouped over time, which might only make sense for trials of equal length (e.g. probe trials).
#'
#' @param data Data set containing at least following columns: "Time", "x", "y", "Animal", "Day", "Trial".
#' "x" and "y" represent the coordinates (position) of the animal at a certain timepoint ("Time") during the trial.
#' @param by Variable to group by. E.g. "Trial", c("Group", "Trial").
#'
#' @export
#' @importFrom dplyr group_by summarize

groupCOORD <- function(data, by) {
  # initiate vars
  Time <- NULL; x <- NULL; y <- NULL

  track_data_average <- data %>%
    dplyr::group_by(Time, !!!syms(by)) %>%
    dplyr::summarize(x = mean(x, na.rm = T), y = mean(y, na.rm=T))

  # save
  filename <- paste("data coordinates averaged (", Sys.Date(), ").csv", sep="")
  write.csv(track_data_average, filename, row.names=FALSE)

  # return
  invisible(track_data_average)
}
