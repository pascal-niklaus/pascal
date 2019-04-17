# R package 'pascal'

## Purpose

This R package contains a collection of functions I programmed
mainly for my own use. I make them available here mainly for my own
collaborators. Some functions are legacy code for which certainly
there are (possibly better) implementations on CRAN.

These functions are reasonably well tested but use them at your own
risk.  Also, I do not provide any support. In case you find a bug,
however, I am happy if you report it.


## Content

I stopped documenting the functions by category, but here is a list of
all objects and functions:

> ls.str("package:pascal")

addColumns : function (trg, src, key, cols, after = TRUE)  
addpoly : function (mem, x, y1, y2)  
aggr : function (d, factors = NULL, newcols = NULL, expand = FALSE, keep.numerics = FALSE)  
angles : Formal class 'standardGeneric' [package "methods"] with 8 slots
aov.ftest : function (aovobj, test.formula, table = FALSE)  
aov.ko : function (formula, data = NULL, projections = FALSE, qr = TRUE, contrasts = NULL, 
    ...)  
apply2D : function (m, x, y = NULL)  
area : Formal class 'standardGeneric' [package "methods"] with 8 slots
arrows2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
as.data.frame : Formal class 'standardGeneric' [package "methods"] with 8 slots
asreml.nvc : function (...)  
binvar : function (x, frac, width = 1)  
catenary : function (x, a = NULL, dx = 1, y1 = 0, y2 = 0, smax = NULL)  
catenary_normal : function (x, a = NULL, dx = 1, y1 = 0, y2 = 0, smax = NULL)  
checkSink : function ()  
circle2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
circumcircle : Formal class 'standardGeneric' [package "methods"] with 8 slots
circumference : Formal class 'standardGeneric' [package "methods"] with 8 slots
cloglog : function (x)  
close_all : function ()  
combn2 : function (x, m, replace = FALSE, FUN = NULL)  
corner.label : function (label = NULL, pos = "topleft", units = "char", dist = 1, fun = NULL, 
    frame = "plot", ...)  
cot : function (z)  
cotpi : function (z)  
current_time : function (fmt = "%d-%b-%Y %H:%M:%S")  
demo2D : function ()  
disableSink : function ()  
E12 :  num [1:12] 1 1.2 1.5 1.8 2.2 2.7 3.3 3.9 4.7 5.6 ...
E24 :  num [1:24] 1 1.1 1.2 1.3 1.5 1.6 1.8 2 2.2 2.4 ...
E48 :  num [1:48] 1 1.05 1.1 1.15 1.21 1.27 1.33 1.4 1.47 1.54 ...
ellipse : function (data = NULL, cov = NULL, center = NULL, level = 0.95, npoints = 100)  
emptyPlot : function (xlim = c(0, 1), ylim = c(0, 1), asp = NA, xpd = NA, xlab = "", 
    ylab = "", bty = "n", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", 
    ...)  
enableSink : function (enabled = TRUE)  
escape : function (x, to.escape = ":")  
finite.only : function (x)  
flushpoly : function (mem)  
getDist : function (d, i, j)  
getObjects : function (cls = "function")  
getResolution : function ()  
gfx2D : function (tx = c(0, 0), ty = tx[2], sx = c(1, 1), sy = sx[2], rot = 0)  
glimpse : function (..., len = 5, pre = 1, post = 0, between = 1)  
greatestCommonDenominator : function (a, b)  
grow : Formal class 'standardGeneric' [package "methods"] with 8 slots
heading : function (txt = NULL, width = 80, right = FALSE, bottom = FALSE, char = "#", 
    spc = "  ", side.char = char, corner.char = rep(char, 4), post = 1, 
    pre = 0, center = FALSE)  
identity2D : function ()  
incircle : Formal class 'standardGeneric' [package "methods"] with 8 slots
indexof : function (d = NULL, col = NULL)  
invcloglog : function (x)  
isect : function (x1, y1, x2, y2, segment = T)  
killfactors : function (d, restore.numerics = FALSE, cols = seq_along(names(d)))  
leastCommonMultiple : function (a, b = NULL)  
lines2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
logstc : function (x, a = 1, b = 1, tau = 1)  
logstc.der : function (x, a = 1, b = 1, tau = 1)  
logstc.der2 : function (x, a = 1, b = 1, tau = 1)  
max_NAsafe : function (x)  
mean_NAsafe : function (x)  
median_NAsafe : function (x)  
min_NAsafe : function (x)  
months.len :  int [1:12] 31 28 31 30 31 30 31 31 30 31 ...
NAtozero : function (x)  
newpoly : function (col = NULL, angle = NULL, density = NULL, lwd = NULL, lty = NULL)  
place : Formal class 'standardGeneric' [package "methods"] with 8 slots
plot : Formal class 'standardGeneric' [package "methods"] with 8 slots
plotci.asreml : function (data, formula = NULL, subset = NULL, backfun = I, xfun = I, pts.col = NULL, 
    pts.pch = 16, pts.cex = 1, fill.col = NULL, line.lty = 1, line.lwd = 1, 
    line.col = NULL, ci.lty = 1, ci.lwd = 1, ci.col = NULL, ci.f = 1)  
plotellipse : function (data, columns = c(1, 2), subset = NULL, level = 0.67, which = c(1, 
    2), fill.ellipse = NA, col.ellipse = "black", col.ma = "black", lwd.ellipse = 1, 
    lwd.ma = 1, lty.ellipse = 1, lty.ma = 1, caps = 0)  
points2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
polyarea : function (x)  
polycircle : function (x, y = NULL, r = NULL, rx = r, ry = rx, pos = "center", phi = 0, 
    n = 16)  
polygon2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
polyrect : function (x, y, w, h, pos = "center", phi = 0)  
polystar : function (x, y = NULL, r = NULL, r1 = r, r2 = r/2, phi = 0, n = 5)  
power.posneg : function (x, posexp = 0.5, negexp = 0.5, slope = 1)  
power.posneg.backtransform : function (x, posexp = 0.5, negexp = 0.5, slope = 1)  
progressBar : function (step = 0)  
progressInit : function (steps = 10, pre = "[", post = "]", done = "*", spc = "-", maxwidth = 60)  
progressPct : function (step = 0)  
progressStep : function ()  
progressStep0 : function ()  
rect2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
reflectx2D : function ()  
reflectxy2D : function ()  
reflecty2D : function ()  
reload.pascal : function ()  
resDivider : function (ratio, rmin = 1000, rmax = 1e+06, series = pascal::E24, exponents = 0:6, 
    n = 2, tol = 0.05, ntop = 10)  
rotate2D : function (theta)  
safen : function (x)  
safeSample : function (x, size, replace = FALSE, prob = NULL)  
scale2D : function (kx, ky = kx)  
sd_NAsafe : function (x)  
se : function (x, na.rm = FALSE)  
se_NAsafe : function (x)  
sgnsqrt : function (x)  
shearx2D : function (phi)  
sheary2D : function (phi)  
show : Formal class 'standardGeneric' [package "methods"] with 8 slots
showSymbols : function ()  
sides : Formal class 'standardGeneric' [package "methods"] with 8 slots
sigStars : function (x)  
simple.heading : function (txt = NULL, width = 80, char = "#", side.char = char, corner.char = rep(char, 
    4), spc = "  ", pre = 0, post = 1, center = TRUE)  
simple.trailer : function (txt = NULL, width = 80, char = "#", side.char = char, corner.char = rep(char, 
    4), spc = "  ", pre = 0, post = 1, center = TRUE)  
Sink : function (...)  
snapToGrid : function (d, theta = 0, sz = 1, which = "points")  
sorted.code : function (..., split = NULL, collapse = NULL)  
splitScreen : function (nx = 2, ny = 1, gapx = 0, gapy = 0, topy = 0, bottomy = 0.2, 
    leftx = 0.2, rightx = 0, byrow = TRUE, debug = FALSE)  
splittxt : function (x, n = 80)  
splt : function (d, by, to.split, factors = NULL, new.names = NULL, expand = FALSE, 
    sep = ":")  
stck : function (d, factors = NULL, covars = NULL, to.stack, cat.names = NULL, 
    expand = FALSE)  
suc : function (x)  
test.asreml : function (d.asr, returnWald = FALSE, silent = FALSE)  
text2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
trailer : function (txt = NULL, width = 80, right = FALSE, char = "#", spc = "  ", 
    side.char = char, corner.char = rep(char, 4), post = 1, pre = 0, center = FALSE)  
transform2D : Formal class 'standardGeneric' [package "methods"] with 8 slots
transformation : Formal class 'standardGeneric' [package "methods"] with 8 slots
transformation<- : Formal class 'standardGeneric' [package "methods"] with 8 slots
translate2D : function (x, y)  
triangle : function (sides = rep(NA, 3), angles = rep(NA, 3), x = NULL, y = NULL, 
    degrees = TRUE, simplify = TRUE)  
trim.ws : function (x)  
unescape : function (x)  
var_NAsafe : function (x)  
vector.crossprod : function (a, b)  
vector.norm : function (x, p = 2)  
xtreme : function (x, f = 3.5)  
xy.errbar : function (x, y, yplus = NULL, yminus = NULL, xplus = NULL, xminus = NULL, 
    yerr = NULL, xerr = NULL, cap = 0.015, lty = 1, lwd = 1, add = FALSE, 
    ...)  
Zr : function (F, dendf, df = 1)  


### Deprecated

I'll remove these functions eventually, since they are remainings from old code.

- niceDiag: Draw path diagram from sem object, using graphviz 
- indexof

### Installation

* download the ready-built package from the [pkgs directory](https://github.com/pascal-niklaus/pascal/tree/master/pkgs)
* use `install_github`:  
`library(devtools)`  
`install_github("pascal-niklaus/pascal/pascal")`


