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
#' @param platform_colour Colour of the platform. Name or hexadecimal code (e.g.: #FF1020). Default = "grey"
#' @param alpha_platform Alpha level for platform. Default = 1
#' @param platform_linetype Linetype for platform. Derived from ggplot2. Default = "solid"
#' @param platform_line_colour Colour of platform circle. Default = "black"
#' @param heatmap_low Low range colour heatmap. Default = "yellow"
#' @param heatmap_high High range colour heatmap. Default = "red"
#' @param loop Loop the animation, default = FALSE
#' @param width Width of the animation (px), default = 480
#' @param height Height of the animation (px), default = 480
#' @param duration Duration of the animation(s), default = 10
#' @param frames Number of frames in the animation, default = 100
#' @param resolution Resolution of GIF, passed to gifski. Default = 80
#' @param theme_settings Optional parameter that passes list of arguments to ggplot2's theme() function.
#' @keywords track velocity distance to target gif
#' @export
#' @import ggplot2
#' @import gifski

heatmapGIF <- function(data, id, day, trial,
                   centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5, ndata_circle=100,
                   platform_colour="grey", alpha_platform=1, platform_linetype="solid", platform_line_colour="black",
                   heatmap_low = "yellow" , heatmap_high = "red",
                   loop = FALSE, width = 480, height = 480, duration = 10, frames = 100, resolution = 80,
                   theme_settings = NULL){
  # read data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial),]

  # initiate vars
  nlevel <- NULL

  # initiate vars
  x <- NULL; y <- NULL; x_coord <- NULL; y_coord <- NULL; Time <- NULL

  # update coordinates (rescale) and add quadrant information
  data <- updateCOORD(data=data,
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


  # outside circle data
  df <- data.frame(x=c(-radius,-radius-radius/10,-radius-radius/10,radius+radius/10,radius+radius/10,-radius-radius/10,-radius-radius/10,-radius),
                   y=c(0,0,radius+radius/10,radius+radius/10,-radius-radius/10,-radius-radius/10,0,0))
  outside_all <- rbind(maze,df)

  # remove datapoints outside maze
  data <- data[which(abs(data$x) <= radius & abs(data$y) <= radius),]

  # parameters
  nframes <- frames
  duration <- duration
  mydelay <- (1/nframes)*duration

  # create plots
  makeplot <- function(){
    # create sequence list
    maxtime <- max(data$Time)
    myseq <- seq(from=1, to=maxtime, length.out = nframes)
    # create list with data frames
    mylist <- list()
    for(i in 1:nframes){
      mylist[[i]] <- data[which(data$Time <= myseq[i]),]
    }
    # create plots
    lapply(mylist, function(mydata){
      plot <- ggplot() +
        # plot heatmap
        stat_density_2d(data=mydata, aes(x = x, y = y, fill = stat(nlevel)), geom = "polygon", na.rm=TRUE) +
        # plot outside_all, cave: set color=NA or otherwise you see the closing line through the circle
        geom_polygon(data=outside_all, aes(x,y), color=NA, fill="white", alpha=1) +
        # plot empty circle
        geom_polygon(data=maze, aes(x,y), color="black", fill=NA, alpha=1) +
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
        geom_polygon(data=platform_circle, aes(x, y), color=platform_line_colour, fill=platform_colour, alpha=alpha_platform, linetype=platform_linetype) +
        # set scales
        scale_x_continuous(breaks = c(-radius,0,radius)) +
        scale_y_continuous(breaks = c(-radius,0,radius)) +
        # scale
        scale_fill_gradient("Density", low=heatmap_low, high=heatmap_high) +
        # theme + fixed coord
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),
              legend.position = "none") +
        if(!is.null(theme_settings)) {do.call(theme,theme_settings)}
        coord_fixed()
      print(plot)
    })
  }

  # create gif
  filename  <- paste("heatmapGIF_", id, "-day_", day, "-trial_", trial ,".gif", sep="")
  gifski::save_gif(makeplot(), gif_file = filename, res = resolution,  width = width, height = height, delay = mydelay, loop = loop)
}
