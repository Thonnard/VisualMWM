
<style>
body {
text-align: justify}
</style>

<!-- README.md is generated from README.Rmd. -->

# VisualMWM <img src="man/figures/heatmapGIF_2b-day_1-trial_1.gif" align="right" width="160"/>

This R package provides tools to visualize Morris water maze tracking
data motion-wise\! The goal is to start from raw, unprocessed data,
i.e. x and y coordinates for every time point, that is obtained with
tracking software (e.g. Ethovision). This package comes with an example
data set (track\_data) that can be used to test the various plotting
options. Plots are generated with
[ggplot2](https://ggplot2.tidyverse.org/), and animations are rendered
with [gganimate](https://gganimate.com/articles/gganimate.html) and
[gifski](https://gif.ski/).

## Installation

``` r
# install
devtools::install_github("Thonnard/VisualMWM")
# load
library(VisualMWM)
```

## Here we go…

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

## What else…
