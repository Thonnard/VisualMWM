#' update rescales x and y coordinates to center (0,0) and adds quadrant information to the data set
#' Adjacent Left is the quadrant to the left of the target quadrant when standing at the target quadrant and looking towards the platform
#' Adjacent Right is the quadrant to the right of the target quadrant when standing at the target quadrant and looking towards the platform
#'
#' @param data Data set containing at following columns: "x", "y".
#' "x" and "y" represent the coordinates (position) of the animal at a certain timepoint during a trial.
#' @param centerx x coordinate of the center of the morris water maze (cm).
#' @param centery y coordinate of the center of the morris water maze (cl).
#' @param radius radius of the morris water maze (cm), default = 75.
#' @param platformx x coordinate of the center of the platform (cm).
#' @param platformy y coordinate of the center of the platform (cm).
#' @param platformradius radius of the platform (cm), default = 7.5.
#' @param removeNA Remove NAs. Default=TRUE.
#' @export

update <- function (data, centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5, removeNA=TRUE) {
  # load data
  data <- as.data.frame(data)

  # initiate vars
  x <- NULL; y <- NULL;  x_coord <- NULL;  y_coord <- NULL;  Time <- NULL

  # adjust coordinates
  data$x_coord <- data$x - centerx
  data$y_coord <- data$y - centery
  platformx_coord <- platformx-centerx
  platformy_coord <- platformy-centery

  # remove NA
  if(isTRUE(removeNA)) {data <- data[complete.cases(data[,c("x_coord", "y_coord")]),]} else {data <- data}

  # set platform quadrant position
  if(platformx_coord<0 & platformy_coord>0) {
    platform_position <- "top_left"
  } else if (platformx_coord>0 & platformy_coord>0) {
    platform_position <- "top_right"
  } else if (platformx_coord<0 & platformy_coord<0) {
    platform_position <- "bottom_left"
  } else {
    platform_position <- "bottom_right"
  }

  # set quadrants
  if (platform_position=="top_left") {
    data$Quadrant[data$x_coord<0 & data$y_coord>0] <- "Target"
    data$Quadrant[data$x_coord>0 & data$y_coord>0] <- "Adjacent Left"
    data$Quadrant[data$x_coord<0 & data$y_coord<0] <- "Adjacent Right"
    data$Quadrant[data$x_coord>0 & data$y_coord<0] <- "Opposite"
  } else if (platform_position=="top_right"){
    data$Quadrant[data$x_coord<0 & data$y_coord>0] <- "Adjacent Right"
    data$Quadrant[data$x_coord>0 & data$y_coord>0] <- "Target"
    data$Quadrant[data$x_coord<0 & data$y_coord<0] <- "Opposite"
    data$Quadrant[data$x_coord>0 & data$y_coord<0] <- "Adjacent Left"
  } else if (platform_position=="bottom_left"){
    data$Quadrant[data$x_coord<0 & data$y_coord>0] <- "Adjacent Left"
    data$Quadrant[data$x_coord>0 & data$y_coord>0] <- "Opposite"
    data$Quadrant[data$x_coord<0 & data$y_coord<0] <- "Target"
    data$Quadrant[data$x_coord>0 & data$y_coord<0] <- "Adjacent Right"
  } else if (platform_position=="bottom_right"){
    data$Quadrant[data$x_coord<0 & data$y_coord>0] <- "Opposite"
    data$Quadrant[data$x_coord>0 & data$y_coord>0] <- "Adjacent Right"
    data$Quadrant[data$x_coord<0 & data$y_coord<0] <- "Adjacent Left"
    data$Quadrant[data$x_coord>0 & data$y_coord<0] <- "Target"
  } else {data$Quadrant = NA}

  # remove NA (when x=0 and y=0, very unlikely)
  if (isTRUE(removeNA)) {data <- data[complete.cases(data[,c("Quadrant")]),]} else {data <- data}
}
