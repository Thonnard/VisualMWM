% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targetdistanceGIF.R
\name{targetdistanceGIF}
\alias{targetdistanceGIF}
\title{Morris water maze distance to target GIF}
\usage{
targetdistanceGIF(
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
  target_colour = "#CDB99C",
  target_linetype = "solid",
  target_line_colour = "black",
  target_alpha = 0.5,
  target_point_colour = "black",
  show_time = FALSE,
  plot_original_target = FALSE,
  original_platformx = NULL,
  original_platformy = NULL,
  original_target_colour = "lightblue",
  original_target_linetype = "dashed",
  original_target_line_colour = "black",
  original_target_alpha = 0.5,
  original_target_point_colour = "black",
  loop = FALSE,
  width = 480,
  height = 480,
  fps = 10,
  duration = 10,
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

\item{target_colour}{Colour of the plotted area. Default = "#CDB99C"}

\item{target_linetype}{Linetype. Default = "solid"}

\item{target_line_colour}{Colour of the line. Default = "black"}

\item{target_alpha}{Alpha of the area. Default = 0.5}

\item{target_point_colour}{Colour of the point. Default = "black"}

\item{show_time}{Shows trial time (s) as subtitle (ggplot). Default = FALSE}

\item{plot_original_target}{Plots the distance to the original target (e.g. for reversal trials). Default = FALSE}

\item{original_platformx}{x coordinate of the center of the original platform (cm). Ignored if plot_original_target = FALSE}

\item{original_platformy}{y coordinate of the center of the original platform (cm). Ignored if plot_original_target = FALSE}

\item{original_target_colour}{Colour of the plotted area. Ignored if plot_original_target = FALSE. Default = "lightblue"}

\item{original_target_linetype}{Linetype. Ignored if plot_original_target = FALSE. Default = "dashed"}

\item{original_target_line_colour}{Colour of the line. Ignored if plot_original_target = FALSE. Default = "black"}

\item{original_target_alpha}{Alpha of the area. Ignored if plot_original_target = FALSE. Default = 0.5}

\item{original_target_point_colour}{Colour of the point. Ignored if plot_original_target = FALSE. Default = "black"}

\item{loop}{Loop the animation, default = FALSE}

\item{width}{Width of the animation (px), default = 480}

\item{height}{Height of the animation (px), default = 480}

\item{fps}{Frames per second of the animation. Minimum 100 frames per GIF. default = 10}

\item{duration}{Duration of the animation (s), default = 10}

\item{theme_settings}{Optional parameter that passes list of arguments to ggplot2's theme() function.}

\item{title}{Add title to GIF. Default = NA}
}
\description{
The targetdistanceGIF function creates GIF showing the distance to target over time.
}
\keyword{distance}
\keyword{gif}
\keyword{maze}
\keyword{morris}
\keyword{reversal}
\keyword{target}
\keyword{to}
\keyword{water}
