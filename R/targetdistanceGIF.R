#' @title Morris water maze distance to target GIF
#'
#' @description The targetdistanceGIF function creates GIF showing the distance to target over time.
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
#' @param target_colour Colour of the plotted area. Default = "#CDB99C"
#' @param target_linetype Linetype. Default = "solid"
#' @param target_line_colour Colour of the line. Default = "black"
#' @param target_alpha Alpha of the area. Default = 0.5
#' @param target_point_colour Colour of the point. Default = "black"
#' @param show_time Shows trial time (s) as subtitle (ggplot). Default = FALSE
#' @param plot_original_target Plots the distance to the original target (e.g. for reversal trials). Default = FALSE
#' @param original_platformx x coordinate of the center of the original platform (cm). Ignored if plot_original_target = FALSE
#' @param original_platformy y coordinate of the center of the original platform (cm). Ignored if plot_original_target = FALSE
#' @param original_target_colour Colour of the plotted area. Ignored if plot_original_target = FALSE. Default = "lightblue"
#' @param original_target_linetype Linetype. Ignored if plot_original_target = FALSE. Default = "dashed"
#' @param original_target_line_colour Colour of the line. Ignored if plot_original_target = FALSE. Default = "black"
#' @param original_target_alpha Alpha of the area. Ignored if plot_original_target = FALSE. Default = 0.5
#' @param original_target_point_colour Colour of the point. Ignored if plot_original_target = FALSE. Default = "black"
#' @param loop Loop the animation, default = FALSE
#' @param width Width of the animation (px), default = 480
#' @param height Height of the animation (px), default = 480
#' @param fps Frames per second of the animation. Minimum 100 frames per GIF. default = 10
#' @param duration Duration of the animation (s), default = 10
#' @param theme_settings Optional parameter that passes list of arguments to ggplot2's theme() function.
#' @param title Add title to GIF. Default = NA
#' @keywords distance to target morris water maze reversal gif
#' @export
#' @import ggplot2
#' @importFrom gganimate animate anim_save gifski_renderer transition_reveal
#' @import gifski

targetdistanceGIF <- function(data, id, day, trial,
                              centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5,
                              target_colour = "#CDB99C", target_linetype = "solid", target_line_colour="black", target_alpha=0.5, target_point_colour="black",
                              show_time = FALSE, plot_original_target = FALSE, original_platformx=NULL, original_platformy=NULL,
                              original_target_colour = "lightblue", original_target_linetype = "dashed",
                              original_target_line_colour="black", original_target_alpha=0.5, original_target_point_colour="black",
                              loop = FALSE, width = 480, height = 480, fps = 10, duration = 10,
                              theme_settings = NULL, title = NA){
  # data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial),]

  # initiate vars
  Time <- NULL

  # update coordinates (rescale) and add quadrant information
  data <- updateCOORD(data=data,
                      centerx=centerx, centery=centery, radius=radius,
                      platformx=platformx, platformy=platformy, platformradius=platformradius, removeNA=TRUE)

  # set platform coordinates
  platformx_coord <- platformx-centerx
  platformy_coord <- platformy-centery

  # set original platform coordinates (optional)
  if(isTRUE(plot_original_target)) {
    if(is_null(original_platformx) | is_null(original_platformy)) {
      original_platformx_coord <- -platformx_coord
      original_platformy_coord <- -platformy_coord
    } else {
      original_platformx_coord <- original_platformx-centerx
      original_platformy_coord <- original_platformy-centery}
  }

  # remove NA
  data <- data[complete.cases(data[,c("x_coord", "y_coord")]),]

  # calculate distance to target
  DistanceToTarget <- NULL
  for(i in 1:nrow(data)) {
    data$DistanceToTarget[i] <- sqrt(sum((data$x_coord[i]-platformx_coord)^2, (data$y_coord[i]-platformy_coord)^2)) - platformradius
    if(data$DistanceToTarget[i] < 0) {data$DistanceToTarget[i] <- 0}
  }

  # calculate distance to original target (optional)
  if(isTRUE(plot_original_target)) {
    DistanceToOriginalTarget <- NULL
    for(i in 1:nrow(data)) {
      data$DistanceToOriginalTarget[i] <- sqrt(sum((data$x_coord[i]-original_platformx_coord)^2, (data$y_coord[i]-original_platformy_coord)^2)) - platformradius
      if(data$DistanceToOriginalTarget[i] < 0) {data$DistanceToOriginalTarget[i] <- 0}
    }
  }

  # plot
  p <- ggplot(data) +
    # distance to target
    geom_segment(aes(x=Time, y=DistanceToTarget, xend = max(Time), yend = DistanceToTarget), linetype = 2, colour = 'grey') +
    geom_point(aes(x=Time, y=DistanceToTarget), size = 2, colour=target_point_colour) +
    geom_area(aes(x=Time, y=DistanceToTarget), fill=target_colour, linetype=target_linetype, color=target_line_colour, alpha=target_alpha) +
    geom_text(aes(y=DistanceToTarget, x = max(Time)+1, label = paste("Target\n", round(DistanceToTarget,1), ' cm')), hjust = 0, fontface=2)

  # plot distance to original platform (optional)
  if(isTRUE(plot_original_target)) {
    p <- p + geom_segment(aes(x=Time, y=DistanceToOriginalTarget, xend = max(Time), yend = DistanceToOriginalTarget), linetype = 2, colour = 'grey') +
      geom_point(aes(x=Time, y=DistanceToOriginalTarget), size = 2, colour=original_target_point_colour) +
      geom_area(aes(x=Time, y=DistanceToOriginalTarget), fill=original_target_colour, linetype=original_target_linetype, color=original_target_line_colour, alpha=original_target_alpha) +
      geom_text(aes(y=DistanceToOriginalTarget, x = max(Time)+1, label = paste("Original\n target\n", round(DistanceToOriginalTarget,1), ' cm')), hjust = 0, fontface=2) +
      geom_segment(aes(x = 0, y = mean(DistanceToOriginalTarget), xend = max(Time), yend = mean(DistanceToOriginalTarget)), linetype="dashed", size=0.5) +
      annotate(geom="text", x=0.5, y=mean(data$DistanceToOriginalTarget)-3,
               label=paste("Distance to original target (mean):", round(mean(data$DistanceToOriginalTarget),1)," cm"), hjust = 0, fontface=2)
  }

  # display mean distance to target (always on top)
  p <- p +
    geom_segment(aes(x = 0, y = mean(DistanceToTarget), xend = max(Time), yend = mean(DistanceToTarget)), linetype="solid", size=0.5) +
    annotate(geom="text", x=0.5, y=mean(data$DistanceToTarget)-3,
             label=paste("Distance to target (mean):", round(mean(data$DistanceToTarget),1)," cm"), hjust = 0, fontface=2)

  # animate and settings
  p <- p +
    # animate
    gganimate::transition_reveal(along=Time) +
    # settings
    coord_cartesian(clip = 'off') +
    ylim(0,150) +
    ylab("Distance to target (cm)") +
    xlab("Time (s)") +
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
  filename <- paste("targetdistance_", id, "-day_", day, "-trial_", trial ,".gif", sep="")
  gganimate::animate(p, renderer = gifski_renderer(loop = loop), width = width, height = height, fps = fps, duration = duration)
  gganimate::anim_save(filename)
}
