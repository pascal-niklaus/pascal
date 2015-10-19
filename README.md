# R package 'pascal'

## Purpose

This R package contains some "convenience" functions I programmed for my own use. 
I make them available here mainly for my own collaborators. 
They are reasonably well tested but use them at your own risk. 
Also, I do not provide any support. In case you find a bug, however, I am happy if you report it. 

## Groups of functions 

### Re-shaping data frames

The following functions were written long ago, and work very well. 
Today, some of this functionality can by achieved using other packages, e.g. plyr.

- aggr: Aggregate a data frame. Very generic.
- splt: Split data frame columns. Very generic, opposite of stck 
- stck: Stack data frame columns. Very generic, opposite of splt

### ANOVA

- aov.ftest: Calculate custom F-tests using lm/aov, choosing non-standard error strata. 
Outputs nicely formatted table.
- aov.ko: Short for aov(terms(..., keep.order),...)

### ASReml

- asreml.nvc: fit a model using asreml, but allowing for negative variance components. 
  This is a convenience function acting as wrapper around asreml, achieving what otherwise has to be done in several steps (i.e. constructing a suitable G.param structure passed to asreml)
- test.asreml: show Wald tests with denominator degrees of freedom, variance components, and (if available) stratum variances,
 given a fitted asreml object

### Formatting

- heading, trailer, simple.heading, simple.trailer: Convenience functions to provide "nice" outputs
- Sink, enableSink, disableSink, checkSink: Turn on and off sink()s
- current_time

### Plotting

- xy.errbar: Add error bars to your graphics
- newpoly/addpoly/flushpoly: intelligently shade area between lines
- showSymbols: show plot symbols

### Functions

- max_NAsafe, mean_NAsafe, median_NAsafe, min_NAsafe, sd_NAsafe, se_NAsafe, var_NAsafe: convenience replacements for anonymous functions removing NAs
- NAtozero: return vector with NA and NaNs zeroed
- finite.only: returns finite values only
- escape/unescape: escape/unescape specified characters by hex codes
- logstc: Three-parameter logistic (sigmoidal) function
- power.posneg, power.posneg.backtransform: power functions, joining different "legs" for positive and negative values
- safen: safely convert to numeric; works for text and factors 
- se: standard error (standard deviation of mean)
- sgnsqrt
- xtreme: Identify extreme values based on `mad`
- trim.ws: remove whitespace 
- sigStars: translate P values to "stars" indicating significance levels
- suc: short for `sort(unique(as.character(x)))`
- isect: intersection of lines, with options
- Sink, enableSink: sink function that can be globally enabled/disabled
- getDist: extract distances from triangular distance matrices
- Zr: Z transformation
- cloglog and invcloglog: complementary log log and its inverse

### Misc

- close_all: closes all sinks and devices
- getObjects
- months.len
- getResolution: Get screen resolution (uses xrandr, works only under Linux)

### Deprecated

I'll remove these functions eventually, since they are remainings from old code.

- niceDiag: Draw path diagram from sem object, using graphviz
- indexof

### Installation

* download the ready-built package from the [pkgs directory](https://github.com/pascal-niklaus/pascal/tree/master/pkgs)
* use `install_github`:  
`library(devtools)`  
`install_github("pascal-niklaus/pascal/pascal")`


