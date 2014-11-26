library(pascal)

## use data set "CO2" as test case

data(CO2)
d<-CO2;

## we need the replicate irrespective of treatment

d$Replicate<-substr(as.character(d$Plant),3,3)
d.split <- splt(d,factors=c("Replicate","Type","conc"),by="Treatment",to.split=c("uptake"))

orig_unique_item  <- paste(d$Replicate,d$Type,d$conc,d$Treatment,sep="|")
split_unique_item <- paste(d.split$Replicate,d.split$Type,d.split$conc,sep="|")

idx.chilled <- match(paste(split_unique_item,"chilled",sep="|"),orig_unique_item)
idx.nonchilled <- match(paste(split_unique_item,"nonchilled",sep="|"),orig_unique_item)

## test if all data correct

stopifnot(all.equal(d.split$uptake.chilled,d$uptake[idx.chilled]))
stopifnot(all.equal(d.split$uptake.nonchilled,d$uptake[idx.nonchilled]))


## now re-stack the data
d.stacked <- stck(d.split,
                  factors=c("Replicate","Type","conc"),
                  to.stack=c("uptake=uptake.chilled,uptake.nonchilled"),
                  cat.names=c("Treatment=chilled,nonchilled"))
d.stacked$conc <- safen(d.stacked$conc)
d.stacked <- d.stacked[order(d.stacked$Replicate,d.stacked$Type,d.stacked$Treatment,d.stacked$conc),]

orig_unique_item  <- paste(d$Replicate,d$Type,d$conc,d$Treatment,sep="|")
stacked_unique_item <- paste(d.stacked$Replicate,d.stacked$Type,d.stacked$conc,d.stacked$Treatment,sep="|")
idx <- match(stacked_unique_item,orig_unique_item)

stopifnot(d.stacked$uptake == d$uptake[idx]);
