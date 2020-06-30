
<!-- README.md is generated from README.Rmd. -->

# VisualMWM <img src="man/figures/logo.gif" align="right" width="5%"/>

This R package provides functions to visualize [Morris water
maze](https://en.wikipedia.org/wiki/Morris_water_navigation_task)
tracking data in motion\! The goal is to visualize behaviour starting
from raw, unprocessed data, i.e. x and y coordinates for every time
point, that is obtained with tracking software (e.g. Ethovision). This
package comes with an example data set (track\_data) that can be used to
test the various plotting options. Plots are generated with
[ggplot2](https://ggplot2.tidyverse.org/), and animations are rendered
with [gganimate](https://gganimate.com/articles/gganimate.html) and
[gifski](https://gif.ski/).

<br>

## Let’s start with an example…

<img src="man/figures/example1.gif" width="100%" />

<br> On the left we see the track of the animal in the maze. This
example shows the second trial of the first day of reversal, which
explains the searching behaviour close to the original target (top left
circle). The GIF in the middle shows the distance to both the original
as well as the new target. The GIF on the right displays the velocity of
the animal during the trial. These GIFs have been stitched together with
the appendGIFs function. <br>

## Installation

``` r
# install
devtools::install_github("Thonnard/VisualMWM")
# load
library(VisualMWM)
```

## Exemplary data set

``` r
# check data
head(track_data)
```

    ##   Time       x       y Animal Day Trial Group
    ## 1 0.00 95.4989 12.2106     1w   1     1     A
    ## 2 0.04      NA      NA     1w   1     1     A
    ## 3 0.08      NA      NA     1w   1     1     A
    ## 4 0.12 95.4611 12.3342     1w   1     1     A
    ## 5 0.16 95.5266 12.0585     1w   1     1     A
    ## 6 0.20 95.5048 12.1942     1w   1     1     A

<br>

## Heatmaps

There are two main types of heatmaps available: raster and contour. Here
we see an example of a raster heatmap (interpolated) on the left, and a
(filled) contour heatmap on the right. Heatmaps are plotted 2D [kernel
density
estimations](https://en.wikipedia.org/wiki/Kernel_density_estimation).

<img src="man/figures/heatmap_example.gif" width="80%" style="display: block; margin: auto;" />

### Another example…

Several other options are available to plot heatmaps. Documentation of
all VisualMWM functions can be found
[here](https://rdrr.io/github/Thonnard/VisualMWM/man/).

<img src="man/figures/heatmap_example2.gif" width="40%" style="display: block; margin: auto;" />

``` r
heatmapGIF(data=track_data, id="2b", day=1, trial=1, 
           centerx=19.4, centery=-1.4, platformx=50.60, platformy=-33.34,
           type="contour", contour_filled=FALSE, contour_colour_scaled=TRUE, 
           heatmap_low = "brown", heatmap_high = "blue",
           theme_settings = list(axis.text = element_text(face="bold", color="brown", size=16)),
           loop = TRUE)
```

<br>

## Tracking

Track GIFs display the animal’s track over time. Several parameters can
be adjusted (e.g. quadrant colours, platform linestyle, track colour,
etc.).

<img src="man/figures/track_example1.gif" width="40%" style="display: block; margin: auto;" />

``` r
trackGIF(data=track_data, id="2b", day=1, trial=2,
         centerx=19.4, centery=-1.49, platformx=50.60, platformy=-33.34, 
         platform_alpha=1, platform_colour="grey", track_colour="darkblue", track_alpha=0.35, 
         quadrant_colours=c("white", "white", "white", "lightblue"), quadrants_alpha = 0.4,
         theme_settings = list(axis.text = element_text(face="bold", color="black", size=16)),
         plot_original_platform = TRUE, original_platform_colour=NA, loop = TRUE)
```

<br>

## Distance to target

Typically, trial duration (latency to target) is the first behavioural
readout that is checked when analyzing Morris water maze data. However,
this parameter is correlated with the animal’s velocity. In case there
is a group difference in velocity, it is a good idea to check the
distance to target as well.

Next example shows the distance to target on the first and fourth trial
during the first day of reversal. These GIFs clearly show that the
animal spent more time in the vicinity of the original platform,
compared to the reversal platform, on the first trial, but not on the
fourth.

<img src="man/figures/distance_example.gif" width="80%" style="display: block; margin: auto;" />

``` r
# First day of reversal, trial 1
targetdistanceGIF(data=track_data, id="2rb", day=1, trial=1, 
                  centerx=19.4, centery=-1.49, platformx=50.60, platformy=-33.34,
                  show_time = TRUE, plot_original_target = TRUE, loop = TRUE,
                  title="Distance to target (reversal, day 1, trial 1)")

# First day of reversal, trial 4
targetdistanceGIF(data=track_data, id="2rb", day=1, trial=4, 
                  centerx=19.4, centery=-1.49, platformx=50.60, platformy=-33.34,
                  show_time = TRUE, plot_original_target = TRUE, loop = TRUE,
                  title="Distance to target (reversal, day 1, trial 4)")
```

<br>

## Velocity

Floating or freezing behaviour can be demonstrated in velocity graphs.
Following GIF shows the difference between normal swimming behaviour
(left) and floating (right).

<img src="man/figures/velocity.gif" width="80%" style="display: block; margin: auto;" />

``` r
# Normal swimming behaviour
velocityGIF(data=track_data, id="2b", day=1, trial=1, centerx=19.4, centery=-1.4, 
            platformx=50.60, platformy=-33.34, title = "Animal: 2b", loop = TRUE)

# Floating
velocityGIF(data=track_data, id="1rg", day=1, trial=4, centerx=19.4, centery=-1.4, 
            platformx=50.60, platformy=-33.34, title = "Animal: 1rg (floating)", loop = TRUE)
```

<br>

## Time spent per quadrant

Time spent per quadrant can be a good indicator of the learning process.
For instance, during probe trials (i.e., trials where the platform is
absent), the position of the animal is a good measure of reference
memory.

<img src="man/figures/barGIFexample.gif" width="80%" style="display: block; margin: auto;" />

``` r
# Track
trackGIF(data=track_data, id="2rb", day=1, trial=2, centerx=19.4, 
         centery=-1.49, platformx=50.60, platformy=-33.34,
         plot_original_platform = TRUE, show_time = TRUE, loop = TRUE)

# Time spent per quadrant
barGIF(data=track_data, id="2rb", day=1, trial=2, loop = TRUE, 
       centerx=19.4, centery=-1.4, platformx=50.60, platformy=-33.34)
```

VisualMWM also provides functions to display behaviour in static graphs.
Next example shows two area plots of time spent per quadrant. Here we
can clearly see that during the fourth trial of reversal (right),
compared to the first trial (left), relatively more time is spent in the
target quadrant ([viridis colour
scales](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html)).

<img src="man/figures/areaQuadrantTime_trial.jpeg" width="80%" style="display: block; margin: auto;" />

``` r
# Reversal, trial 1
p1 <- areaQuadrantTime(data=track_data, id="1w", day=1, trial=1, device="jpeg", 
                       centerx=19.4, centery=-1.4, radius=75, platformx=50.60, 
                       platformy=-33.34,platformradius=7.5, 
                       title = "Reversal, day 1, trial 1", 
                       theme_settings = list(legend.title = element_text(face="bold", size=14)))

# Reveral, trial 4
p2 <- areaQuadrantTime(data=track_data, id="1w", day=1, trial=4, device="jpeg",
                      centerx=19.4, centery=-1.4, radius=75, platformx=50.60, 
                      platformy=-33.34, platformradius=7.5,
                      title = "Reversal, day 1, trial 4",
                      theme_settings = list(legend.title = element_text(face="bold", size=14)))

# arrange plots with commong legend
plot <- ggpubr::ggarrange(p1, p2, ncol=2, common.legend = TRUE, legend="bottom")

# save
ggplot2::ggsave(filename = "areaQuadrantTime_trial.jpeg", plot = plot, width = 10, height=5)
```

Plots are saved automatically when areaQuadrantTime is called, but this
function also returns the plot as a ggplot object that can be used for
other purposes (e.g. arranging mutliple plots).

<br>

## Merge data

VisualMWM also provides a function to merge raw data files (e.g. of
different subjects, trials,…). This function is written based on
Ethovision raw data output that contains extra information above the
actual tracking data. Here is an example for csv files (examples are
provided in tests/csv\_merge):

``` r
mergeCOORD(startData=39,rowID=34, rowDay=35, rowTrial=32, rowGroup="FOO", filetype = "csv")
```

This code produces an csv output file that contains following
information (group information was added later as an example):

``` r
head(track_data, n=3)
```

    ##   Time       x       y Animal Day Trial Group
    ## 1 0.00 95.4989 12.2106     1w   1     1     A
    ## 2 0.04      NA      NA     1w   1     1     A
    ## 3 0.08      NA      NA     1w   1     1     A

``` r
tail(track_data, n=3)
```

    ##        Time       x        y Animal Day Trial Group
    ## 81887 16.04 55.3735 -30.7304    2br   1     4     B
    ## 81888 16.08 55.3727 -30.7352    2br   1     4     B
    ## 81889 16.12 55.3988 -30.7719    2br   1     4     B

<br>

## Workflow

(under construction…)
