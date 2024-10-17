# GGIRmatcher

<!-- badges: start -->

[![R-CMD-check](https://github.com/PhysicalActivityOpenTools/GGIRmatcher/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PhysicalActivityOpenTools/GGIRmatcher/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of GGIRmatcher is to match time series GGIR part 5 output with
time series extracted from an additional sensor at epoch level.
Additionally, window-level (midnight to midnight, waking-up to
waking-up, sleep onset to sleep onset, and time segments in the day)
output can be derived using the GGIR output and the functions specified
by the user over the additional sensor output.

GGIRmatcher has been extensively tested with CGM output.

## Installation

You can install the development version of GGIRmatcher like so:

``` r
library(remotes)
install_github("DIVAtools/GGIRmatcher")
```

## Disclaimers

At the moment, GGIRmatcher is under development and its functionalities are limited to:

-   It expects that the additional sensor is not GGIR output as well. GGIRmatcher could
be extended to match GGIR output data from different devices, yet this functionality needs
to be developed.

-   It only matches GGIR day-level output from part 5, no person-level estimates or 
estimates from other parts of the GGIR pipeline are included in the GGIRmatcher report.

-   It only handles GGIR output with one configuration (i.e., one light, moderate, and
vigorous threshold, and only one sleep definition).

-   It only derives full window-level outputs with indicators of valid windows for
GGIR output and for additional sensor output. It is the user responsibility to 
clean up those reports and to generate person-level aggregates from them if needed.

-   It does not generate any kind of visualization of the data, but it produces the 
output necessary to visualize data.

If you are interested in promoting or funding any of these or other aspects of 
GGIRmatcher, please get in touch with the package maintainer.
