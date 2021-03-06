% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/velocityGIF.R
\name{velocityGIF}
\alias{velocityGIF}
\title{Morris water maze velocity GIF}
\usage{
velocityGIF(
  data,
  id,
  day,
  trial,
  centerx,
  centery,
  radius = 75,
  platformx,
  platformy,
  platformradius = 7.5,
  target_colour = "#008000",
  target_linetype = "solid",
  target_line_colour = NA,
  target_alpha = 0.5,
  target_point_colour = "#008000",
  removeVelocityOutliers = TRUE,
  SwimspeedLimit = 50,
  loop = FALSE,
  width = 480,
  height = 480,
  fps = 10,
  duration = 10,
  show_time = FALSE,
  theme_settings = NULL,
  title = NA
)
}
\arguments{
\item{data}{Data set containing at least following columns: "Time", "x", "y", "Animal", "Day", "Trial".
"x" and "y" represent the coordinates (position) of the animal at a certain timepoint ("Time") during the trial.}

\item{id}{ID of the animal}

\item{day}{day number}

\item{trial}{trial number}

\item{centerx}{x coordinate of the center of the morris water maze (cm)}

\item{centery}{y coordinate of the center of the morris water maze (cl)}

\item{radius}{radius of the morris water maze (cm), default = 75}

\item{platformx}{x coordinate of the center of the platform (cm)}

\item{platformy}{y coordinate of the center of the platform (cm)}

\item{platformradius}{radius of the platform (cm), default = 7.5}

\item{target_colour}{Colour of the plotted area. Default = "#008000"}

\item{target_linetype}{Linetype. Default = "solid"}

\item{target_line_colour}{Colour of the line. Default = NA}

\item{target_alpha}{Alpha of the area. Default = 0.5}

\item{target_point_colour}{Colour of the point. Default = "#008000"}

\item{removeVelocityOutliers}{Remove velocity outiers (e.g. due to tracking issues). Default = TRUE}

\item{SwimspeedLimit}{Maximum speed (cm/s). Data points above this are removed if removeVelocityOutliers = TRUE. Default = 50}

\item{loop}{Loop the animation, default = FALSE}

\item{width}{Width of the animation (px), default = 480}

\item{height}{Height of the animation (px), default = 480}

\item{fps}{Frames per second of the animation. Minimum 100 frames per GIF. default = 10}

\item{duration}{Duration of the animation (s), default = 10}

\item{show_time}{Shows trial time (s) as subtitle (ggplot). Default = FALSE}

\item{theme_settings}{Optional parameter that passes list of arguments to ggplot2's theme() function.}

\item{title}{Add title to GIF. Default = NA}
}
\description{
The velocityGIF function creates GIF showing the velocity during a trial over time.
}
\keyword{gif}
\keyword{maze}
\keyword{morris}
\keyword{reversal}
\keyword{velocity}
\keyword{water}
