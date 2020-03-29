#' @title Create data set for plotting circles
#'
#' @description The circle function creates a data set with x and y values that can be used as coordinates to plot a circle.
#' The arguments 'from' and 'to' can be adjust to output different quadrants of the circle.
#'
#' @param x The x coordinate of the center of the circle.
#' @param y The y coordinate of the center of the circle.
#' @param radius The radius of the circle.
#' @param nrow_data The number of data points.
#' @param from Start of sequence. Default = 0
#' @param to End of sequence. E.g. End = 0.5 results in a quadrant polygon. Default = 2
#' @param add_center Add the coordinate of the center (x,y) to the data set. Useful when creating quadrant polygons. Default = FALSE
#' @export

circle <- function(x = 0, y = 0, radius = 75, nrow_data = 100, from = 0, to = 2, add_center=FALSE) {
  temp <- seq(from*pi, to*pi, length.out=nrow_data)
  data_circle <- data.frame(x = x + radius * cos(temp), y = y + radius * sin(temp))
  if (isTRUE(add_center)) {data_circle <- rbind(data_circle, c(x,y))} else {data_circle=data_circle}
}
