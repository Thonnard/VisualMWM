
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

<br><br>

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

## Heatmaps

There are two main types of heatmaps available: raster and contour. Here
we can see an….

## Tracking

## Distance to target

## Velocity

## Time spent per quadrant

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
