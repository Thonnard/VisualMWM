#' @title Area chart for percentage of quadrant time
#'
#' @description areaQuadrantTime creates a geom_area plot showing the percentage of time spent per quadrant for a specific trial of one animal, visualized in a stacked area plot.
#' This function returns the graph as a ggplot object and saves the image (default = jpeg).
#'
#' @param data Data set containing at least following columns: "Time", "x", "y", "Animal", "Day", "Trial".
#' "x" and "y" represent the coordinates (position) of the animal at a certain timepoint ("Time") during the trial.
#' @param id ID of the animal
#' @param day day number
#' @param trial trial number
#' @param centerx x coordinate of the center of the morris water maze (cm)
#' @param centery y coordinate of the center of the morris water maze (cl)
#' @param radius radius of the morris water maze (cm), default = 75
#' @param platformx x coordinate of the center of the platform (cm)
#' @param platformy y coordinate of the center of the platform (cm)
#' @param platformradius radius of the platform (cm), default = 7.5
#' @param device Device for the graph. Options: "png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"
#' @param width Width of the graph, default = 18
#' @param height Height of the graph, default = 12
#' @param units Units. Options: "in", "cm", "mm". Default = "cm".
#' @param dpi Resolution. Default = 300
#' @param viridis_color_palette Viridus color palette. Options: "A", "B", "C", "D", "E". Default = "D".
#' @param theme_settings Optional parameter that passes list of arguments to ggplot2's theme() function.
#' @keywords area stacked graph quadrant time percentage
#' @export
#' @import ggplot2
#' @importFrom dplyr group_by summarize
#' @importFrom reshape2 melt
#' @importFrom viridis scale_fill_viridis scale_color_viridis

areaQuadrantTime<- function(data, id, day, trial,
                   centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5,
                   device="jpeg", width=18, height=12, units="cm", dpi=300, viridis_color_palette = "D",
                   theme_settings = NULL){

  # read data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial),]

  # update coordinates (rescale) and add quadrant information
  data <- updateCOORD(data=data,
                      centerx=centerx, centery=centery, radius=radius,
                      platformx=platformx, platformy=platformy, platformradius=platformradius, removeNA=TRUE)

  # calculate duration
  data$Duration <- 0
  split <- split(data, list(data$Animal, data$Day, data$Trial))
  split <- lapply(split, function(x){
    records <- length(x[, "Duration"])
    for(i in 2:records){
      x[, "Duration"][i] <- x[, "Time"][i] - x[, "Time"][i-1]
    }
    x # return data frame
  })
  data <- do.call(rbind, split)
  rownames(data) <- NULL

  # add cumulative time in quadrant
  data$CumQuadrantTime <- ave(data$Duration, data$Quadrant, FUN=cumsum)

  # calculate percentage of time quadrant
  for (i in 1:nrow(data)) {
    if(data$Quadrant[i]=="Target"){
      data$TQ[i] <- data$CumQuadrantTime[i]
    }
    else data$TQ[i] <- 0
    if(data$TQ[i]==0) {data$TQ[i] <- max(data$TQ[0:i])}

    if(data$Quadrant[i]=="Opposite"){
      data$OQ[i] <- data$CumQuadrantTime[i]
    }
    else data$OQ[i] <- 0
    if(data$OQ[i]==0) {data$OQ[i] <- max(data$OQ[0:i])}

    if(data$Quadrant[i]=="Adjacent Left"){
      data$AL[i] <- data$CumQuadrantTime[i]
    }
    else data$AL[i] <- 0
    if(data$AL[i]==0) {data$AL[i] <- max(data$AL[0:i])}

    if(data$Quadrant[i]=="Adjacent Right"){
      data$AR[i] <- data$CumQuadrantTime[i]
    }
    else data$AR[i] <- 0
    if(data$AR[i]==0) {data$AR[i] <- max(data$AR[0:i])}
  }

  data$TQ <- (data$TQ/data$Time)*100
  data$TQ[is.na(data$TQ)] <- 0

  data$OQ <- (data$OQ/data$Time)*100
  data$OQ[is.na(data$OQ)] <- 0

  data$AL <- (data$AL/data$Time)*100
  data$AL[is.na(data$AL)] <- 0

  data$AR <- (data$AR/data$Time)*100
  data$AR[is.na(data$AR)] <- 0

  # initiate vars
  Time <- NULL

  # reshape: wide to long
  data_long <- reshape2::melt(data, id.vars=c("Time"), measure.vars = c("TQ","OQ","AL","AR"), value.name = "Time_Quadrant", variable.name = "Quadrant")

  # aggregate: one value per quadrant per time bin
  data_long_average <- data_long %>%
    dplyr::group_by(Quadrant) %>%
    dplyr::summarize(Mean_Time_Quadrant = mean(Time_Quadrant, na.rm = TRUE))

  # create plot
  p1 <- ggplot(data = data_long, aes(x=Time, y=Time_Quadrant, fill=Quadrant, color=Quadrant)) +
    geom_area() +
    ggtitle(paste("Trial ", trial, sep="")) +
    xlab("Trial duration (s)") +
    ylab("Quadrant Time (%)") +
    viridis::scale_fill_viridis(discrete = TRUE, option = viridis_color_palette) +
    viridis::scale_color_viridis(discrete = TRUE, option = viridis_color_palette) +
    theme_classic() +
    if(!is.null(theme_settings)) {do.call(theme,theme_settings)}

  # save plot
  filename <- paste("areaQuadrantTime_", id, "-day_", day, "-trial_", trial , ".", device, sep="")
  ggsave(plot = p1, filename = filename, device = device, width = width, height = height, dpi = dpi, units = units)

  # return ggplot object
  return(p1)
}
