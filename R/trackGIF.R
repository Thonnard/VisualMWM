#' trackGIF creates a gif showing the track, distance to target and velocity of one specific trial
#'
#' Output: 4 gifs (track, distance to target, velocity and all three combined) test
#' @param data data set containing at least following columns: "Time", "x", "y", "Animal", "Day", "Trial"
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
#' @keywords track velocity distance to target gif
#' @export
#' @import ggplot2
#' @importFrom gganimate animate anim_save transition_reveal gifski_renderer transition_time shadow_trail
#' @import gifski
#' @import ggforce
#' @import magick
#' @import purrr
#' @import stats
#' @import utils

trackGIF <- function(data, id, day, trial,
                     centerx,centery,radius,platformx,platformy,platformradius,
                     removeSwimspeedOutliers = TRUE, SwimspeedLimit = 50){

  # read data
  data <- as.data.frame(data)

  # select data
  # select 1 trial from 1 mouse
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

  # circles data
  circles <- data.frame(
    circle <- c("arena", "platform"),
    x0 <- c(0,platformx-centerx),
    y0 <- c(0,platformy-centery),
    r <- c(radius,platformradius),
    alpha <- c(1, 0.2))
  colnames(circles) <- c("circle","x0","y0","radius", "alpha")

  # remove NA
  data <- data[complete.cases(data[,c("x_scaled", "y_scaled")]),]

  # calculate distance, duration and swimspeed
  writeLines("Calculating swim speed...")
  data$Distance <- 0
  data$Duration <- 0
  data$Swimspeed <- 0
  split <- split(data, list(data$Animal, data$Day, data$Trial))
  split <- lapply(split, function(x){
    records <- length(x[, "Distance"])
    for(i in 2:records){
      x[, "Distance"][i] <- sqrt(sum((x[, "x_scaled"][i]-x[, "x_scaled"][i-1])^2, (x[, "y_scaled"][i]-x[, "y_scaled"][i-1])^2))
      x[, "Duration"][i] <- x[, "Time"][i] - x[, "Time"][i-1]
      x[, "Swimspeed"][i] <- x[, "Distance"][i]/x[, "Duration"][i]
    }
    x # return data frame
  })
  data <- do.call(rbind, split)
  rownames(data) <- NULL

  # add distance to target
  writeLines("Calculating distance to target...")
  data$DistanceToTarget <- 0
  split <- split(data, list(data$Animal, data$Day, data$Trial))
  split <- lapply(split, function(x){
    records <- length(x[, "Distance"])
    for(i in 1:records){
      x[, "DistanceToTarget"][i] <- sqrt(sum((x[, "x_scaled"][i]-platformx_scaled)^2, (x[, "y_scaled"][i]-platformy_scaled)^2))-platformradius
      if (x[, "DistanceToTarget"][i] < 0) {x[, "DistanceToTarget"][i] <- 0}
    }
    x # return data frame
  })
  data <- do.call(rbind, split)
  rownames(data) <- NULL

  # remove swim speed outliers
  if(removeSwimspeedOutliers == TRUE) {data <- data[which(data$Swimspeed <= SwimspeedLimit),]}

  # create dir for all output
  wd <- getwd()
  dir <- paste("trackGIF_", format(Sys.time(), "%F_%H-%M-%S"), sep="")
  dir.create(dir)

  # create and set dir for graphic output
  setwd(dir)
  dir.create("Plots")
  setwd("Plots")

  # plot tracks
  p1 <- ggplot() +
    # plot circle
    geom_circle(data=circles, aes(x0=x0, y0=y0, r=r, fill=circle, alpha=circle), colour="black", size=1, show.legend=FALSE) +
    scale_fill_manual(values=c("white","black"))+
    scale_alpha_manual(values=alpha) +
    # plot track
    geom_point(data = data, aes(x=x_scaled, y=y_scaled), color="orange", alpha=0.35) +
    # plot rectangle
    geom_hline(yintercept=-radius) +
    geom_hline(yintercept=radius) +
    geom_vline(xintercept=-radius) +
    geom_vline(xintercept=radius) +
    # transition
    transition_time(Time) +
    shadow_trail(distance=0.01)+
    # theme settings
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank()) +
    coord_fixed() # to ensure true circularity

  # animate(p1)
  writeLines("\nCreating track image...")
  animate(p1, renderer = gifski_renderer(loop = F))  # no loop
  anim_save("p1.gif")

  # plot distance to target
  p2 <- ggplot(data, aes(x=Time, y=DistanceToTarget, group=Trial)) +
    geom_line() +
    geom_segment(aes(xend = max(Time), yend = DistanceToTarget), linetype = 2, colour = 'grey') +
    geom_point(size = 2) +
    geom_text(aes(x = max(Time)+1, label = paste("Trial",Trial)), hjust = 0) +
    transition_reveal(along=Time) +
    coord_cartesian(clip = 'off') +
    ylim(0,150) +
    geom_segment(aes(x = 0, y = mean(DistanceToTarget), xend = max(Time), yend = mean(DistanceToTarget)), linetype="dotted", size=1) +
    annotate(geom="text", x=0, y=mean(data$DistanceToTarget)-2, label=paste(round(mean(data$DistanceToTarget),1)," cm"), hjust = 0) +
    labs(title = paste('Animal ', data$Animal[1], ' - Distance to target'), y = 'Distance to target (cm)', x = 'Time (s)') +
    theme_minimal() +
    theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

  #animate(p2)
  writeLines("\n\nCreating distance to target image...")
  animate(p2, renderer = gifski_renderer(loop = F))   # no loop
  anim_save("p2.gif")

  # plot swim speed
  p3 <- ggplot(data, aes(x=Time, y=Swimspeed, group=Trial)) +
    geom_line() +
    geom_segment(aes(xend = max(Time), yend = Swimspeed), linetype = 2, colour = 'grey') +
    geom_point(size = 2) +
    geom_text(aes(x = max(Time)+1, label = paste("Trial",Trial)), hjust = 0) +
    transition_reveal(along=Time) +
    coord_cartesian(clip = 'off') +
    ylim(0,50) +
    geom_segment(aes(x = 0, y = mean(Swimspeed), xend = max(Time), yend = mean(Swimspeed)), linetype="dotted", size=1) +
    annotate(geom="text", x=0, y=mean(data$Swimspeed)-1, label=paste(round(mean(data$Swimspeed),1)," cm/s"), hjust = 0) +
    labs(title = paste('Animal ', data$Animal[1], ' - Swim speed'), y = 'Swim speed (cm/s)', x = 'Time (s)') +
    theme_minimal() +
    theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

  #animate(p3)
  writeLines("\n\nCreating swim speed image...")
  animate(p3, renderer = gifski_renderer(loop = F))   # no loop
  anim_save("p3.gif")

  # # Read the three plots and append together (in 2 steps)
  # writeLines("\n\nAppending images...")
  # filename <- paste(id, " day_", day, " trial_", trial, ".gif")
  # map2(
  #   "p1.gif" %>% image_read() %>% as.list(),
  #   "p2.gif" %>% image_read() %>% as.list(),
  #   ~image_append(c(.x, .y))
  # ) %>%
  #   lift(image_join)(.) %>%
  #   image_write("p1_p2.gif")
  #
  # map2(
  #   "p1_p2.gif" %>% image_read() %>% as.list(),
  #   "p3.gif" %>% image_read() %>% as.list(),
  #   ~image_append(c(.x, .y))
  # ) %>%
  #   lift(image_join)(.) %>%
  #   image_write(filename)
  # file.remove("p1_p2.gif")
  #
  #
  #
  # # show result
  # invisible(capture.output(image_read(filename)))

  # set original wd
  setwd(wd)
  writeLines("\nDone!")
}
