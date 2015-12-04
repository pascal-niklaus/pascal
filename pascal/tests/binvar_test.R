library(pascal)


stopifnot(all.equal(binvar(0,1,1),1/12))
stopifnot(all.equal(binvar(1,1,1),1/12))
stopifnot(all.equal(binvar(0,1,2),4/12))
stopifnot(all.equal(binvar(c(0,.5),c(1,1),1/2),1/12))    

