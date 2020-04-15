#' @title Morris water maze track data GIF
#'
#' @description The trackGIF function creates a GIF showing the track of the animal for a specific trial.
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
#' @param quadrants_alpha Alpha level for quadrants. Default = 0.2
#' @param platform_alpha Alpha level for platform. Default = 1
#' @param platform_colour Colour of the platform. Name or hexadecimal code (e.g.: #FF1020). Default = "black"
#' @param platform_line_size Size of platform circle. Default = 0.5
#' @param platform_linetype Linetype of platform circle. Default = "solid"
#' @param platform_line_colour Colour of platform circle line. Default = "black"
#' @param track_colour Colour of the track line. Name or hexadecimal code (e.g.: #FF1020). Default = orange
#' @param track_alpha Alpha level for the track line. Default = 0.35
#' @param loop Loop the animation, default = FALSE
#' @param width Width of the animation (px), default = 480
#' @param height Height of the animation (px), default = 480
#' @param fps Frames per second of the animation. Minimum 100 frames per GIF. default = 10
#' @param duration Duration of the animation(s), default = 10
#' @param theme_settings Optional parameter that passes list of arguments to ggplot2's theme() function.
#' @param title Add title to GIF. Default = NA
#' @param plot_original_platform Plot the original platform (for reversal trials). Default = FALSE
#' @param original_platformx x coordinate of the center of the original platform (cm). Ignored if plot_original_platform = FALSE
#' @param original_platformy y coordinate of the center of the original platform (cm). Ignored if plot_original_platform = FALSE
#' @param original_platform_colour Colour of the original platform. Name or hexadecimal code (e.g.: #FF1020). Ignored if plot_original_platform = FALSE. Default = "grey"
#' @param original_platform_alpha Alpha level for original platform. Ignored if plot_original_platform = FALSE. Default = 0.4
#' @param original_platform_linetype Linetype of original platform circle. Ignored if plot_original_platform = FALSE. Default = "dotted"
#' @param original_platform_line_size Size of original platform circle. Ignored if plot_original_platform = FALSE. Default = 0.5
#' @param original_platform_line_colour Colour of original platform circle line. Ignored if plot_original_platform = FALSE. Default = "black"
#' @keywords track morris water maze gif reversal
#' @export
#' @import ggplot2
#' @importFrom gganimate animate anim_save gifski_renderer transition_time shadow_trail
#' @import gifski
#' @import ggforce
#' @import stats

trackGIF <- function(data, id, day, trial,
                     centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5, ndata_circle=100,
                     quadrant_colours=c("white","white","white","white"), quadrants_alpha=0.2,
                     platform_alpha=1,  platform_colour="black", platform_line_size=0.5, platform_linetype="solid", platform_line_colour="black",
                     track_colour="orange", track_alpha=0.35,
                     loop = FALSE, width = 480, height = 480, fps = 10, duration = 10,
                     theme_settings = NULL, title = NA,
                     plot_original_platform = FALSE, original_platformx=NULL, original_platformy=NULL,
                     original_platform_colour="grey", original_platform_alpha=0.4, original_platform_linetype="dotted",
                     original_platform_line_size=0.5, original_platform_line_colour="black"
){
  # read data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial),]

  # initiate vars
  x <- NULL; y <- NULL; x_coord <- NULL; y_coord <- NULL; Time <- NULL

  # update coordinates (rescale) and add quadrant information
  data <- updateCOORD(data=data,
                      centerx=centerx, centery=centery, radius=radius,
                      platformx=platformx, platformy=platformy, platformradius=platformradius, removeNA=TRUE)

  # set platform coordinates
  platformx_coord <- platformx-centerx
  platformy_coord <- platformy-centery

  # set original platform coordinates (optional)
  if(isTRUE(plot_original_platform)) {
    if(is_null(original_platformx) | is_null(original_platformy)) {
      original_platformx_coord <- -platformx_coord
      original_platformy_coord <- -platformy_coord
    } else {
      original_platformx_coord <- original_platformx-centerx
      original_platformy_coord <- original_platformy-centery}
  }

  # create circles and quadrant data
  top_right_quadrant <- circle(x=0, y=0, radius=radius, nrow_data=ndata_circle, from=0, to=0.5, add_center=TRUE)
  bottom_right_quadrant <- circle(x=0, y=0, radius=radius, nrow_data=ndata_circle, from=0, to=-0.5, add_center=TRUE)
  top_left_quadrant <- -bottom_right_quadrant
  bottom_left_quadrant <- -top_right_quadrant
  maze <- circle(x=0, y=0, radius=radius, nrow_data=ndata_circle, from=0, to=2, add_center=FALSE)
  platform_circle <- circle(x=platformx_coord, y=platformy_coord, radius=platformradius, nrow_data=ndata_circle, from=0, to=2, add_center=FALSE)

  # create circle original platform (optional)
  if(isTRUE(plot_original_platform)) {
    original_platform_circle <- circle(x=original_platformx_coord, y=original_platformy_coord, radius=platformradius, nrow_data=ndata_circle, from=0, to=2, add_center=FALSE)
  }

  # plot tracks
  p1 <- ggplot() +
    # colour quadrants
    geom_polygon(data=top_left_quadrant, aes(x,y), color=NA, fill=quadrant_colours[1], alpha=quadrants_alpha) +
    geom_polygon(data=top_right_quadrant, aes(x,y), color=NA, fill=quadrant_colours[2], alpha=quadrants_alpha) +
    geom_polygon(data=bottom_left_quadrant, aes(x,y), color=NA, fill=quadrant_colours[3], alpha=quadrants_alpha) +
    geom_polygon(data=bottom_right_quadrant, aes(x,y), color=NA, fill=quadrant_colours[4], alpha=quadrants_alpha) +
    # plot quadrant division
    geom_segment(aes(x=-radius,xend=radius,y=0,yend=0),linetype=2) +
    geom_segment(aes(x=0,xend=0,y=-radius,yend=radius),linetype=2) +
    # plot rectangle
    geom_hline(yintercept=-radius) +
    geom_hline(yintercept=radius) +
    geom_vline(xintercept=-radius) +
    geom_vline(xintercept=radius) +
    # plot maze
    geom_path(data=maze, aes(x, y), color="black") +
    # plot platform template
    geom_polygon(data=platform_circle, aes(x, y), color="white", fill="white", alpha=1) +  # get white background
    # plot platform
    geom_polygon(data=platform_circle, aes(x, y), color=platform_line_colour, fill=platform_colour, alpha=platform_alpha, linetype=platform_linetype, size=platform_line_size) +
    # plot track
    geom_point(data = data, aes(x=x_coord, y=y_coord), color=track_colour, alpha=track_alpha) +
    # set scales
    scale_x_continuous(breaks = c(-radius,0,radius)) +
    scale_y_continuous(breaks = c(-radius,0,radius)) +
    # transition
    gganimate::transition_time(Time) +
    gganimate::shadow_trail(distance=0.01) +
    # theme + fixed coord
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(face="bold", colour="black", size="14")) +
    coord_fixed(xlim = c(-radius,radius), ylim = c(-radius,radius), expand=TRUE)

  # add original platform (optional)
  if(isTRUE(plot_original_platform)) {
    # plot original platform template
    p1 <- p1 + geom_polygon(data=original_platform_circle, aes(x, y), color="white", fill="white", alpha=1)  # get white background
    # plot original platform
    p1 <- p1 + geom_polygon(data=original_platform_circle, aes(x, y), color=original_platform_line_colour, fill=original_platform_colour, alpha=original_platform_alpha, linetype=original_platform_linetype, size=original_platform_line_size)
    # plot track (again, to plot over template)
    p1 <- p1 + geom_point(data = data, aes(x=x_coord, y=y_coord), color=track_colour, alpha=track_alpha)
  }

  # add title (optional)
  if(!is.na(title)) {p1 <- p1 + ggtitle(title)}

  # update theme settings (optional)
  if(!is.null(theme_settings)) {
    p1 <- p1 + do.call(theme,theme_settings)}

  # update animation parameters
  gganimate::animate(p1, fps = fps, duration = duration, width=width, height=height, renderer = gganimate::gifski_renderer(loop = loop))

  # save animation
  filename <- paste("trackGIF_", id, "-day_", day, "-trial_", trial ,".gif", sep="")
  gganimate::anim_save(filename=filename)
}
