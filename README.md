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

### Plotting

- xy.errbar: Add error bars to your graphics

