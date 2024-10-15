---
editor_options: 
  markdown: 
    wrap: 72
---

# GGIRmatcher

<!-- badges: start -->

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

## Using GGIR matcher

### Pre-requisites

Using GGIRmatcher require that you have previously run GGIR over your
accelerometer data, and additionally that you have cleaned and store
your additional sensor data as RData files.

As additional sensors might have different cleaning necessities, this is
a step that each user should approach based on their needs and
requirements.

The RData files from the additional sensor are expected to contain a
two-column data frame, including: timestamp and the output of interest.

### Step-by-step into GGIRmatcher

#### 1- Match time series at epoch level

``` r
library(GGIRmatcher) 
match_time_series(
  GGIR_output_dir,
  additional_ts_dir,
  outputdir,
  add_metric_name = NA,
  idloc = "_",
  tz = Sys.timezone(),
  overwrite = F,
  verbose = T)
```

#### 2- Aggregate per window

1.  Define functions to apply to the additional sensor data:

Here some examples.

``` r
myfuns = list(mean = function(x) mean(x, na.rm = T),        # mean
              median = function(x) median(x, na.rm = T),    # median
              SD = function(x) sd(x, na.rm = T),            # standard deviation
              CV = function(x) sd(x, na.rm = T) / mean(x, na.rm = T), # coefficient of variation
              nCGM_levels = function(x) table(cut(x, breaks = c(0,54,70,140,180,250,Inf), right = F)), # n recordings in levels of the additional sensor output
              AUC = function(x, time) pracma::trapz(time[!is.na(x)]/60, x[!is.na(x)])) # area under the curve
```

2.  Aggregate per window:

``` r
aggregate_per_window(
  tspath,
  outputdir,
  GGIR_output_dir,
  FUNs = list(n = function(x) sum(!is.na(x)), mean = mean),
  qwindow = NULL,
  qwindow_names = NULL,
  overwrite = FALSE,
  tz = Sys.timezone(),
  verbose = TRUE)
```

#### 3- Extract full window-level report

``` r
full_window_report(
  outputdir,
  GGIR_output_dir = NULL,
  includecrit_day_spt = NULL,
  includecrit_day = NULL,
  includecrit_spt = NULL,
  additional_sf_seconds = NULL,
  minimum_WW_OO_length = 0,
  minimum_segments_length = 0,
  verbose = TRUE,
  ...)
```

## The output of GGIRmatcher

It mimics GGIR output folder structure

meta folder

-   ms5.out

-   ms5.outraw

results folder

-   QC

    -   full window-level report (MM, WW, OO, Segments)

-   variableDictionary (user is responsible for filling up the
    additional sensor output descriptions)

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