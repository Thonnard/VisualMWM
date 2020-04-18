#' @title Morris water maze velocity GIF
#'
#' @description The velocityGIF function creates GIF showing the velocity during a trial over time.
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
#' @param target_colour Colour of the plotted area. Default = "#008000"
#' @param target_linetype Linetype. Default = "solid"
#' @param target_line_colour Colour of the line. Default = NA
#' @param target_alpha Alpha of the area. Default = 0.5
#' @param target_point_colour Colour of the point. Default = "#008000"
#' @param removeVelocityOutliers Remove velocity outiers (e.g. due to tracking issues). Default = TRUE
#' @param SwimspeedLimit Maximum speed (cm/s). Data points above this are removed if removeVelocityOutliers = TRUE. Default = 50
#' @param loop Loop the animation, default = FALSE
#' @param width Width of the animation (px), default = 480
#' @param height Height of the animation (px), default = 480
#' @param fps Frames per second of the animation. Minimum 100 frames per GIF. default = 10
#' @param duration Duration of the animation (s), default = 10
#' @param show_time Shows trial time (s) as subtitle (ggplot). Default = FALSE
#' @param theme_settings Optional parameter that passes list of arguments to ggplot2's theme() function.
#' @param title Add title to GIF. Default = NA
#' @keywords velocity morris water maze reversal gif
#' @export
#' @import ggplot2
#' @importFrom gganimate animate anim_save gifski_renderer transition_reveal
#' @import gifski

velocityGIF <- function(data, id, day, trial,
                        centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5,
                        target_colour = "#008000", target_linetype = "solid", target_line_colour=NA, target_alpha=0.5, target_point_colour="#008000",
                        removeVelocityOutliers = TRUE, SwimspeedLimit = 50,
                        loop = FALSE, width = 480, height = 480, fps = 10, duration = 10,
                        show_time = FALSE, theme_settings = NULL, title = NA){

  # data
  data <- as.data.frame(data)

  # initiate vars
  Time <- NULL
  Velocity <- NULL

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial & data$Day == day),]

  # update coordinates (rescale) and add quadrant information
  data <- updateCOORD(data=data,
                      centerx=centerx, centery=centery, radius=radius,
                      platformx=platformx, platformy=platformy, platformradius=platformradius, removeNA=TRUE)

  # remove NA
  data <- data[complete.cases(data[,c("x_coord", "y_coord")]),]

  # calculate distance to target
  data$Distance <- 0
  data$Duration <- 0
  data$Velocity <- 0
  for(i in 2:nrow(data)) {
    data$Distance[i] <- sqrt(sum((data$x_coord[i]-data$x_coord[i-1])^2, (data$y_coord[i]-data$y_coord[i-1])^2))
    data$Duration[i] <- data$Time[i] - data$Time[i-1]
    data$Velocity[i] <- data$Distance[i]/data$Duration[i]
  }

  # set last data point to zero
  data$Velocity[max(nrow(data))] <- 0

  # remove velocity outliers
  if(removeVelocityOutliers == TRUE) {data <- data[which(data$Velocity <= SwimspeedLimit),]}

  # plot
  p <- ggplot(data) +
    # velocity
    geom_segment(aes(x=Time, y=Velocity, xend = max(Time), yend = Velocity), linetype = 2, colour = 'grey') +
    geom_point(aes(x=Time, y=Velocity), size = 2, colour=target_point_colour) +
    geom_area(aes(x=Time, y=Velocity), fill=target_colour, linetype=target_linetype, color=target_line_colour, alpha=target_alpha) +
    geom_text(aes(y=Velocity, x = max(Time)+1, label = paste("Velocity\n", round(Velocity,1), ' cm/s')), hjust = 0, fontface=2) +
    geom_segment(aes(x = 0, y = mean(Velocity), xend = max(Time), yend = mean(Velocity)), linetype="dashed", size=0.5) +
    annotate(geom="text", x=0.5, y=mean(data$Velocity)-1, label=paste("Velocity (mean):", round(mean(data$Velocity),1)," cm/s"), hjust = 0, fontface=2) +
    ylab("Velocity (cm/s)") +
    xlab("Time (s)") +
    # animate
    gganimate::transition_reveal(along=Time) +
    # settings
    coord_cartesian(clip = 'off') +
    ylim(0,50) +
    theme_classic() +
    theme(legend.position = "none", axis.text = element_text(size=12, face = "bold", colour = "black"),
          axis.title = element_text(size=16, face="bold", colour="black"),  plot.title = element_text(face="bold"),
          plot.margin = margin(5.5, 40, 5.5, 5.5), plot.subtitle = element_text(face="bold", size="12"))

  # show time (optional)
  if(isTRUE(show_time)) {
    p <- p + labs(subtitle = paste("Time: ", "{round(as.numeric(frame_along),1)}","s", sep=""))
  }

  # add title (optional)
  if(!is.na(title)) {p <- p + ggtitle(title)}

  # update theme settings (optional)
  if(!is.null(theme_settings)) {
    p <- p + do.call(theme,theme_settings)}

  # save
  filename <- paste("velocity_", id, "-day_", day, "-trial_", trial ,".gif", sep="")
  gganimate::animate(p, renderer = gifski_renderer(loop = loop), width = width, height = height, fps = fps, duration = duration)
  gganimate::anim_save(filename)

}


