#' trackGIF creates a GIF showing the track of the animal for a specific trial
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
#' @param ndata_circle Number of data points in the circle data set. Higher means smoother (more perfect) circle. Default = 100
#' @param quadrant_colours Fill colours of quadrants. Order = top left, top right, bottom left, bottom right. Name or hexadecimal code (e.g.: #FF1020). Default = c("white","white","white","white")
#' @param platform_colour Colour of the platform. Name or hexadecimal code (e.g.: #FF1020). Default = "grey"
#' @param alpha_quadrants Alpha level for quadrants. Default = 0.2
#' @param alpha_platform Alpha level for platform. Default = 1
#' @param track_colour Colour of the track line. Name or hexadecimal code (e.g.: #FF1020). Default = orange
#' @param alpha_track Alpha level for the track line. Default = 0.35
#' @param loop Loop the animation, default = FALSE
#' @param width Width of the animation (px), default = 480
#' @param height Height of the animation (px), default = 480
#' @param fps Frames per second of the animation, default = 10
#' @param duration Duration of the animation(s), default = 10
#' @param frames Number of frames in the animation, default = 100
#' @param time_bins Number of time-bins in the animation, default = 50
#' @keywords track velocity distance to target gif
#' @export
#' @import ggplot2
#' @importFrom gganimate animate anim_save transition_reveal gifski_renderer transition_time shadow_trail
#' @import gifski
#' @import ggforce
#' @import stats

trackGIF <- function(data, id, day, trial,
                   centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5, ndata_circle=100,
                   quadrant_colours=c("white","white","white","white"), platform_colour="grey", alpha_quadrants=0.2, alpha_platform=1,
                   track_colour="orange", alpha_track=0.35,
                   loop = FALSE, width = 480, height = 480, fps = 10, duration = 10, frames = 100, time_bins = 50){
  # read data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial),]

  # initiate vars
  x <- NULL; y <- NULL; x_coord <- NULL; y_coord <- NULL; Time <- NULL

  # update coordinates (rescale) and add quadrant information
  data <- update(data=data,
                 centerx=centerx, centery=centery, radius=radius,
                 platformx=platformx, platformy=platformy, platformradius=platformradius, removeNA=TRUE)

  # set platform coordinates
  platformx_coord <- platformx-centerx
  platformy_coord <- platformy-centery

  # create circles and quadrant data
  top_right_quadrant <- circle(x=0, y=0, radius=radius, nrow_data=ndata_circle, from=0, to=0.5, add_center=TRUE)
  bottom_right_quadrant <- circle(x=0, y=0, radius=radius, nrow_data=ndata_circle, from=0, to=-0.5, add_center=TRUE)
  top_left_quadrant <- -bottom_right_quadrant
  bottom_left_quadrant <- -top_right_quadrant
  maze <- circle(x=0, y=0, radius=radius, nrow_data=ndata_circle, from=0, to=2, add_center=FALSE)
  platform_circle <- circle(x=platformx_coord, y=platformy_coord, radius=platformradius, nrow_data=ndata_circle, from=0, to=2, add_center=FALSE)

  # plot tracks
  p1 <- ggplot() +
    # colour quadrants
    geom_polygon(data=top_left_quadrant, aes(x,y), color=NA, fill=quadrant_colours[1], alpha=alpha_quadrants) +
    geom_polygon(data=top_right_quadrant, aes(x,y), color=NA, fill=quadrant_colours[2], alpha=alpha_quadrants) +
    geom_polygon(data=bottom_left_quadrant, aes(x,y), color=NA, fill=quadrant_colours[3], alpha=alpha_quadrants) +
    geom_polygon(data=bottom_right_quadrant, aes(x,y), color=NA, fill=quadrant_colours[4], alpha=alpha_quadrants) +
    # plot quadrant division
    geom_segment(aes(x=-radius,xend=radius,y=0,yend=0),linetype=2) +
    geom_segment(aes(x=0,xend=0,y=-radius,yend=radius),linetype=2) +
    # plot rectangle
    geom_hline(yintercept=-radius) +
    geom_hline(yintercept=radius) +
    geom_vline(xintercept=-radius) +
    geom_vline(xintercept=radius) +
    # plot circle and platform
    geom_path(data=maze, aes(x, y), color="black") +
    geom_polygon(data=platform_circle, aes(x, y), color="black", fill="white", alpha=1) +  # get white background
    geom_polygon(data=platform_circle, aes(x, y), color="black", fill=platform_colour, alpha=alpha_platform) +
    # plot track
    geom_point(data = data, aes(x=x_coord, y=y_coord), color=track_colour, alpha=alpha_track) +
    # transition
    transition_time(Time) +
    shadow_trail(distance=0.01) +
    # theme + fixed coord
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank()) +
    coord_fixed()

  # update animation parameters
  animate(p1, nframes = frames, fps = fps, duration = duration, width=width, height=height, renderer = gifski_renderer(loop = loop))

  # save animation
  filename <- paste("trackGIF_", id, "-day_", day, "-trial_", trial ,".gif", sep="")
  anim_save(filename=filename)
}
