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
#' @param remove_data_outside_maze Remove datapoints that lie outside the water maze. Default = TRUE
#' @param platform_colour Colour of the platform. Name or hexadecimal code (e.g.: #FF1020). Default = NA
#' @param platform_alpha Alpha level for platform. Default = 1
#' @param platform_linetype Linetype for platform. Derived from ggplot2. Default = "dotted"
#' @param platform_line_colour Colour of platform circle. Default = "black"
#' @param platform_line_size Size of platform line. Default = 1
#' @param heatmap_low Low range colour heatmap. Default = "yellow"
#' @param heatmap_high High range colour heatmap. Default = "red"
#' @param type Type of heatmap. Options are "raster" or "contour". Default = "raster"
#' @param interpolate Interpolate raster heatmaps? Ignored for contour heatmaps. Default = TRUE
#' @param contour_filled Fill contour heatmaps? Ignored for raster heatmaps. Default = TRUE
#' @param contour_colour_scaled Colour scale for contour heatmaps. When false, colour of contour lines is set by contour_colour.
#' When TRUE, colour scale is set by heatmap_low and heatmap_high. Default = FALSE
#' @param contour_colour_filled Colour of contour lines in filled contour heatmap. Default = NA
#' @param contour_colour Colour of contour lines in empty contour heatmap. Default = "blue"
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
                   remove_data_outside_maze=TRUE,
                   platform_colour=NA, platform_alpha=1, platform_linetype="dotted", platform_line_colour="black", platform_line_size=1,
                   heatmap_low = "yellow" , heatmap_high = "red",
                   type="raster", interpolate=TRUE, contour_filled=TRUE, contour_colour_scaled=FALSE,
                   contour_colour_filled = NA, contour_colour = "blue",
                   loop = FALSE, width = 480, height = 480, duration = 10, frames = 100, resolution = 80,
                   theme_settings = NULL){
  # read data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial),]

  # initiate vars
  ..level.. <- NULL
  ..density.. <- NULL

  # initiate vars
  x <- NULL; y <- NULL; x_coord <- NULL; y_coord <- NULL; Time <- NULL

  # update coordinates (rescale) and add quadrant information
  data <- updateCOORD(data=data,
                 centerx=centerx, centery=centery, radius=radius,
                 platformx=platformx, platformy=platformy, platformradius=platformradius, removeNA=TRUE)

  # set platform coordinates
  platformx_coord <- platformx-centerx
  platformy_coord <- platformy-centery

  # create maze and platform circles
  maze <- circle(x=0, y=0, radius=radius, nrow_data=ndata_circle, from=0, to=2, add_center=FALSE)
  platform_circle <- circle(x=platformx_coord, y=platformy_coord, radius=platformradius, nrow_data=ndata_circle, from=0, to=2, add_center=FALSE)


  # outside circle data
  df <- data.frame(x=c(-radius,-radius-radius/10,-radius-radius/10,radius+radius/10,radius+radius/10,-radius-radius/10,-radius-radius/10,-radius),
                   y=c(0,0,radius+radius/10,radius+radius/10,-radius-radius/10,-radius-radius/10,0,0))
  outside_all <- rbind(maze,df)

  # remove datapoints outside maze
  if (isTRUE(remove_data_outside_maze)) {
    data <- data[which(abs(data$x) <= radius & abs(data$y) <= radius),]} else {data <- data}

  # parameters
  nframes <- frames
  duration <- duration
  mydelay <- (1/nframes)*duration

  # create raster plots
  makeRASTER <- function(){
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
      plot <- ggplot(data=mydata, aes(x=x, y=y)) +
        # heatmap
        stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, interpolate = interpolate) +
        # plot outside_all, cave: set color=NA or otherwise you see the closing line through the circle
        geom_polygon(data=outside_all, aes(x,y), color=NA, fill="white", alpha=1) +
        # scales
        scale_x_continuous(breaks = c(-radius,0,radius)) +
        scale_y_continuous(breaks = c(-radius,0,radius)) +
        # plot quadrant division
        geom_segment(aes(x=-radius,xend=radius,y=0,yend=0),linetype=2) +
        geom_segment(aes(x=0,xend=0,y=-radius,yend=radius),linetype=2) +
        # plot rectangle
        geom_hline(yintercept=-radius) +
        geom_hline(yintercept=radius) +
        geom_vline(xintercept=-radius) +
        geom_vline(xintercept=radius) +
        # maze
        #geom_path(data=maze, aes(x, y), color="black") +
        # platform
        geom_polygon(data=platform_circle, aes(x, y), color=platform_line_colour, fill=platform_colour, alpha=platform_alpha, linetype=platform_linetype, size=platform_line_size) +
        # colours
        scale_fill_gradient("Density", low=heatmap_low, high=heatmap_high) +
        # theme + coord
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),
              legend.position = "none") +
        coord_fixed(xlim = c(-radius,radius), ylim = c(-radius,radius), expand=TRUE)
      if(!is.null(theme_settings)) {
        plot_adj <- plot + do.call(theme,theme_settings)
        print(plot_adj)} else {print(plot)}
    })
  }

  # create filled contour plots
  makeCONTOUR_filled <- function(){
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
      plot <- ggplot(data=mydata, aes(x=x, y=y)) +
        # heatmap
        stat_density_2d(aes(fill = ..level..), geom = "polygon", colour=contour_colour_filled) +
        # plot outside_all, cave: set color=NA or otherwise you see the closing line through the circle
        geom_polygon(data=outside_all, aes(x,y), color=NA, fill="white", alpha=1) +
        # scales
        scale_x_continuous(breaks = c(-radius,0,radius)) +
        scale_y_continuous(breaks = c(-radius,0,radius)) +
        # plot quadrant division
        geom_segment(aes(x=-radius,xend=radius,y=0,yend=0),linetype=2) +
        geom_segment(aes(x=0,xend=0,y=-radius,yend=radius),linetype=2) +
        # plot rectangle
        geom_hline(yintercept=-radius) +
        geom_hline(yintercept=radius) +
        geom_vline(xintercept=-radius) +
        geom_vline(xintercept=radius) +
        # maze
        geom_path(data=maze, aes(x, y), color="black") +
        # platform
        geom_polygon(data=platform_circle, aes(x, y), color=platform_line_colour, fill=platform_colour, alpha=platform_alpha, linetype=platform_linetype, size=platform_line_size) +
        # colours
        scale_fill_gradient("Density", low=heatmap_low, high=heatmap_high) +
        # theme + coord
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),
              legend.position = "none") +
        coord_fixed(xlim = c(-radius,radius), ylim = c(-radius,radius), expand=TRUE)
      if(!is.null(theme_settings)) {
        plot_adj <- plot + do.call(theme,theme_settings)
        print(plot_adj)} else {print(plot)}
    })
  }

  # create empty contour plots
  makeCONTOUR_empty <- function(){
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
      plot <- ggplot(data=mydata, aes(x=x, y=y)) +
        # heatmap
        geom_density_2d(colour=contour_colour) +
        # plot outside_all, cave: set color=NA or otherwise you see the closing line through the circle
        geom_polygon(data=outside_all, aes(x,y), color=NA, fill="white", alpha=1) +
        # scales
        scale_x_continuous(breaks = c(-radius,0,radius)) +
        scale_y_continuous(breaks = c(-radius,0,radius)) +
        # plot quadrant division
        geom_segment(aes(x=-radius,xend=radius,y=0,yend=0),linetype=2) +
        geom_segment(aes(x=0,xend=0,y=-radius,yend=radius),linetype=2) +
        # plot rectangle
        geom_hline(yintercept=-radius) +
        geom_hline(yintercept=radius) +
        geom_vline(xintercept=-radius) +
        geom_vline(xintercept=radius) +
        # maze
        geom_path(data=maze, aes(x, y), color="black") +
        # platform
        geom_polygon(data=platform_circle, aes(x, y), color=platform_line_colour, fill=platform_colour, alpha=platform_alpha, linetype=platform_linetype, size=platform_line_size) +
        # theme + coord
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),
              legend.position = "none") +
        coord_fixed(xlim = c(-radius,radius), ylim = c(-radius,radius), expand=TRUE)
      if(!is.null(theme_settings)) {
        plot_adj <- plot + do.call(theme,theme_settings)
        print(plot_adj)} else {print(plot)}
    })
  }

  # create empty contour plots with low-high colours
  makeCONTOUR_empty_scale <- function(){
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
      plot <- ggplot(data=mydata, aes(x=x, y=y)) +
        # heatmap
        geom_density_2d(aes(colour=..level..)) +
        # plot outside_all, cave: set color=NA or otherwise you see the closing line through the circle
        geom_polygon(data=outside_all, aes(x,y), color=NA, fill="white", alpha=1) +
        # scales
        scale_x_continuous(breaks = c(-radius,0,radius)) +
        scale_y_continuous(breaks = c(-radius,0,radius)) +
        # plot quadrant division
        geom_segment(aes(x=-radius,xend=radius,y=0,yend=0),linetype=2) +
        geom_segment(aes(x=0,xend=0,y=-radius,yend=radius),linetype=2) +
        # plot rectangle
        geom_hline(yintercept=-radius) +
        geom_hline(yintercept=radius) +
        geom_vline(xintercept=-radius) +
        geom_vline(xintercept=radius) +
        # maze
        geom_path(data=maze, aes(x, y), color="black") +
        # platform
        geom_polygon(data=platform_circle, aes(x, y), color=platform_line_colour, fill=platform_colour, alpha=platform_alpha, linetype=platform_linetype, size=platform_line_size) +
        # colours
        scale_colour_gradient("Density", low=heatmap_low, high=heatmap_high) +
        # theme + coord
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),
              legend.position = "none") +
        coord_fixed(xlim = c(-radius,radius), ylim = c(-radius,radius), expand=TRUE)
      if(!is.null(theme_settings)) {
        plot_adj <- plot + do.call(theme,theme_settings)
        print(plot_adj)} else {print(plot)}
    })
  }

  # create filename
  filename  <- paste("heatmapGIF_", id, "-day_", day, "-trial_", trial ,".gif", sep="")

  # create gif
  writeLines("This might take a while...")
  if(type=="raster") {
    gifski::save_gif(makeRASTER(), gif_file = filename, res = resolution,  width = width, height = height, delay = mydelay, loop = loop)
  } else if (type=="contour"){
    if(isTRUE(contour_filled)) {
      gifski::save_gif(makeCONTOUR_filled(), gif_file = filename, res = resolution,  width = width, height = height, delay = mydelay, loop = loop)
    } else {
      if(isTRUE(contour_colour_scaled)) {
        gifski::save_gif(makeCONTOUR_empty_scale(), gif_file = filename, res = resolution,  width = width, height = height, delay = mydelay, loop = loop)
      } else {gifski::save_gif(makeCONTOUR_empty(), gif_file = filename, res = resolution,  width = width, height = height, delay = mydelay, loop = loop)}
    }
  } else {stop("Set type as 'raster' or 'contour'.")}
}
