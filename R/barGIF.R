#' barGIF creates a GIF showing the percentage of time spent per quadrant for one trial of one animal
#'
#' @param data Data set containing at least following columns: "Time", "x", "y", "Animal", "Day", "Trial".
#' "x" and "y" represent the coordinates (position) of the animal at a certain timepoint ("Time") during the trial.
#' @param id ID of the animal
#' @param day day number
#' @param trial trial number
#' @param centerx x coordinate of the center of the morris water maze
#' @param centery y coordinate of the center of the morris water maze
#' @param radius radius of the morris water maze
#' @param platformx x coordinate of the center of the platform
#' @param platformy y coordinate of the center of the platform
#' @param platformradius radius of the platform
#' @param removeSwimspeedOutliers removes swim speed outliers (>= SwimspeedLimit), default = TRUE
#' @param SwimspeedLimit default = 50 (cm/s), only used when removeSwimspeedOutliers = TRUE
#' @param loop Loop the animation, default = FALSE
#' @param width Width of the animation in pixels, default = 480
#' @param height Height of the animation in pixels, default = 480
#' @param fps Frames per second of the animation, default = 10
#' @param duration Duration of the animation in seconds, default = 10
#' @param frames Number of frames in the animation, default = 100
#' @param time_bins Number of time-bins in the animation, default = 50
#' @keywords bar graph quadrant time
#' @export
#' @import ggplot2
#' @importFrom gganimate animate anim_save transition_reveal gifski_renderer transition_time shadow_trail
#' @import gifski
#' @import ggforce
#' @importFrom dplyr group_by summarize
#' @import reshape2

barGIF <- function(data, id, day, trial,
                     centerx, centery, radius, platformx, platformy, platformradius,
                     removeSwimspeedOutliers = TRUE, SwimspeedLimit = 50,
                     loop = FALSE, width = 480, height = 480, fps = 10, duration = 10, frames = 100, time_bins = 50){

  # read data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial),]

  # rescale
  centerx=centerx
  centery=centery
  data$x_scaled <- data$x - centerx
  data$y_scaled <- data$y - centery
  radius=radius
  platformx=platformx
  platformx_scaled <- platformx-centerx
  platformy=platformy
  platformy_scaled <- platformy-centery
  platformradius=platformradius

  # remove NA
  data <- data[complete.cases(data[,c("x_scaled", "y_scaled")]),]

  # set platform quadrant position
  if(platformx_scaled<0 & platformy_scaled>0) {
    platform_position <- "top_left"
  } else if (platformx_scaled>0 & platformy_scaled>0) {
    platform_position <- "top_right"
  } else if (platformx_scaled<0 & platformy_scaled<0) {
    platform_position <- "bottom_left"
  } else {
    platform_position <- "bottom_right"
  }

  # set quadrants
  if (platform_position=="top_left") {
    data$Quadrant[data$x_scaled<0 & data$y_scaled>0] <- "Target"
    data$Quadrant[data$x_scaled>0 & data$y_scaled>0] <- "Adjacent Left"
    data$Quadrant[data$x_scaled<0 & data$y_scaled<0] <- "Adjacent Right"
    data$Quadrant[data$x_scaled>0 & data$y_scaled<0] <- "Opposite"
  } else if (platform_position=="top_right"){
    data$Quadrant[data$x_scaled<0 & data$y_scaled>0] <- "Adjacent Right"
    data$Quadrant[data$x_scaled>0 & data$y_scaled>0] <- "Target"
    data$Quadrant[data$x_scaled<0 & data$y_scaled<0] <- "Opposite"
    data$Quadrant[data$x_scaled>0 & data$y_scaled<0] <- "Adjacent Left"
  } else if (platform_position=="bottom_left"){
    data$Quadrant[data$x_scaled<0 & data$y_scaled>0] <- "Adjacent Left"
    data$Quadrant[data$x_scaled>0 & data$y_scaled>0] <- "Opposite"
    data$Quadrant[data$x_scaled<0 & data$y_scaled<0] <- "Target"
    data$Quadrant[data$x_scaled>0 & data$y_scaled<0] <- "Adjacent Right"
  } else if (platform_position=="bottom_right"){
    data$Quadrant[data$x_scaled<0 & data$y_scaled>0] <- "Opposite"
    data$Quadrant[data$x_scaled>0 & data$y_scaled>0] <- "Adjacent Right"
    data$Quadrant[data$x_scaled<0 & data$y_scaled<0] <- "Adjacent Left"
    data$Quadrant[data$x_scaled>0 & data$y_scaled<0] <- "Target"
  } else {data$Quadrant = NA}

  # remove NA (when x=0 and y=0, very unlikely)
  data <- data[complete.cases(data[,c("Quadrant")]),]

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

  # reshape: wide to long
  data_long <- melt(data, id.vars=c("Time"), measure.vars = c("TQ","OQ","AL","AR"), value.name = "Time_Quadrant", variable.name = "Quadrant")

  # create time bins
  data_long$Time_bin <- cut_number(data_long$Time, time_bins)

  # aggregate: one value per quadrant per time bin
  data_long_average <- data_long %>%
    group_by(Time_bin,Quadrant) %>%
    summarize(Mean_Time_Quadrant = mean(Time_Quadrant, na.rm = TRUE))

  # create plot
  p1 <- ggplot(data_long_average) +
    geom_bar(stat="identity", aes(x=Quadrant, y=Mean_Time_Quadrant, fill=Quadrant)) +
    geom_text(aes(x=Quadrant, label = round(Mean_Time_Quadrant), y = Mean_Time_Quadrant, color=Quadrant),
              position = position_dodge(0.9), vjust = -1, size=4, fontface="bold") +
    geom_hline(yintercept=25, linetype="dashed", size=0.2) +
    labs(title = "Time-bin: {closest_state}") +
    # scale_fill_manual(values = c("grey", "black", "yellow", "red")) +
    scale_x_discrete(name="Quadrant", breaks=c("TQ","OQ","AL","AR"), labels=c("Target", "Opposite", "Adjacent\nLeft", "Adjacent\nRight")) +
    ylab("Time in quadrant (%)") +
    theme_classic() +
    theme(legend.position = "none", axis.text = element_text(size=12, face = "bold", colour = "black"),
          axis.title = element_text(size=16, face="bold", colour="black"),  plot.title = element_text(face="bold")) +
    transition_states(Time_bin, transition_length = 1) +
    ease_aes('sine-in-out')

  # create animation
  animate(p1, nframes = frames, fps = fps, duration = duration, width=width, height=height, renderer = gifski_renderer(loop = loop))

  # save animation
  filename <- paste("barGIF_", id, "-day_", day, "-trial_", trial ,".gif", sep="")
  anim_save(filename=filename)
}
