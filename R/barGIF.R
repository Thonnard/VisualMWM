#' @title Bar chart for percentage of quadrant time
#'
#' @description The barGIF function creates a GIF showing the percentage of time spent per quadrant for a specific trial of one animal.
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
#' @param bar_colours Colours of the four bars. Default = c("#F8766D","#7CAE00", "#00BFC4", "#C77CFF")
#' @param show_time Show time-bins (s) as subtitle (ggplot). Default = FALSE
#' @param loop Loop the animation, default = FALSE
#' @param width Width of the animation (px), default = 480
#' @param height Height of the animation (px), default = 480
#' @param fps Frames per second of the animation, default = 10
#' @param duration Duration of the animation(s), default = 10
#' @param frames Number of frames in the animation, default = 100
#' @param time_bins Number of time-bins in the animation, default = 50
#' @param theme_settings Optional parameter that passes list of arguments to ggplot2's theme() function.
#' @param title Add title to GIF. Default = NA
#' @keywords bar graph quadrant time
#' @export
#' @import ggplot2
#' @importFrom gganimate animate anim_save gifski_renderer transition_states ease_aes
#' @import gifski
#' @import ggforce
#' @importFrom dplyr group_by summarize
#' @importFrom reshape2 melt

barGIF <- function(data, id, day, trial,
                     centerx, centery, radius = 75, platformx, platformy, platformradius = 7.5,
                     bar_colours=c("#F8766D","#7CAE00", "#00BFC4", "#C77CFF"), show_time=FALSE,
                     loop = FALSE, width = 480, height = 480, fps = 10, duration = 10, frames = 100, time_bins = 50,
                     theme_settings = NULL, title=NA){

  # read data
  data <- as.data.frame(data)

  # select data
  data <- data[which(data$Animal == id & data$Trial == trial & data$Day == day),]

  # update coordinates (rescale) and add quadrant information
  data <- updateCOORD(data=data,
                 centerx=centerx, centery=centery, radius=radius,
                 platformx=platformx, platformy=platformy, platformradius=platformradius, removeNA=TRUE)

  # calculate duration
  data$Duration <- 0
  for(i in 2:nrow(data)){
    data$Duration[i] <- data$Time[i] - data$Time[i-1]
  }

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
  data_long <- reshape2::melt(data, id.vars=c("Time"), measure.vars = c("TQ","OQ","AL","AR"), value.name = "Time_Quadrant", variable.name = "Quadrant")

  # create time bins
  data_long$Time_bin <- cut_number(data_long$Time, time_bins)

  # aggregate: one value per quadrant per time bin
  data_long_average <- data_long %>%
    dplyr::group_by(Time_bin,Quadrant) %>%
    dplyr::summarize(Mean_Time_Quadrant = mean(Time_Quadrant, na.rm = TRUE))

  # create plot
  p1 <- ggplot(data_long_average) +
    # bar graph
    geom_bar(stat="identity", aes(x=Quadrant, y=Mean_Time_Quadrant, fill=Quadrant)) +
    # text labels above bars
    geom_text(aes(x=Quadrant, label = round(Mean_Time_Quadrant), y = Mean_Time_Quadrant, colour=Quadrant),
              position = position_dodge(0.9), vjust = -1, size=4, fontface="bold") +
    # 25% chance level
    geom_hline(yintercept=25, linetype="dashed", size=0.2) +
    # adjust bar and text colours
    scale_fill_manual(values = bar_colours) +
    scale_colour_manual(values = bar_colours) +
    # adjust titles
    scale_x_discrete(name="Quadrant", breaks=c("TQ","OQ","AL","AR"), labels=c("Target", "Opposite", "Adjacent\nLeft", "Adjacent\nRight")) +
    ylab("Time in quadrant (%)") +
    # transition
    gganimate::transition_states(Time_bin, transition_length = 1) +
    gganimate::ease_aes('sine-in-out') +
    theme_classic() +
    theme(legend.position = "none", axis.text = element_text(size=12, face = "bold", colour = "black"),
          axis.title = element_text(size=16, face="bold", colour="black"),  plot.subtitle = element_text(face="bold"))

  # show time (optional)
  if(isTRUE(show_time)) {p1 <- p1 + labs(subtitle = "Time-bin: {closest_state}")}

  # show title (optional)
  if(!is.na(title)) {p1 <- p1 + ggtitle(title)}

  # update theme settings (optional)
  if(!is.null(theme_settings)) {
    p1 <- p1 + do.call(theme,theme_settings)}

  # update animation parameters
  animate(p1, nframes = frames, fps = fps, duration = duration, width=width, height=height, renderer = gifski_renderer(loop = loop))

  # save animation
  filename <- paste("barGIF_", id, "-day_", day, "-trial_", trial ,".gif", sep="")
  anim_save(filename=filename)
}
