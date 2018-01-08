## this script creates three maps and corresponding histograms for three 
## different classification schemes: quantiles, standard deviation, and equal
## interval. each scheme has five classes. each class is coded with the same
## color between each map and histogram pair, to demonstrate how each scheme
## creates different sized and spaced breaks in the data.

## doing this for raster maps is more complicated than for shapefiles. to
## implement this concept for shapefiles, see https://github.com/pysal/legendgram
## and https://mathewkiang.com/2017/01/16/using-histogram-legend-choropleths/


## quantiles - equally filled but unevenly spaced bins

## standard deviation - middle bin centered on the mean with evenly-spaced bins 
## according to standard deviation distances in both directions from the mean. 
## works best with normally-distributed data

## equal interval - equalled distanced but unevenly filled bins


## load required packages
install.packages(c("raster", "viridis", "classInt"))
library(raster)
library(viridis)
library(classInt)
library(ggplot2)

## download MDAT Mammal Summary Products data from:
## http://seamap.env.duke.edu/models/mdat/ 
## put MDAT_MammalModels_SummaryProducts_v1_1 in working dir
## work with Total abundance of ESA-listed species as example
## or pick any raster dataset

ESA_total <-raster("~/MDAT_MammalModels_SummaryProducts_v1_1/commondata
                   /raster_data/mammal_abundance_All_ESA_listed_absolute.tif")


## create an object for the raster layer values
ESA.values <-values(ESA_total)


## create objects for the raster layer values for each type of classification: 
## quantiles, standard deviation, equal interval
## give n value of number of classes desired

breaks.quant <-classIntervals(ESA.values, n=5, style="quantile", 
                              intervalClosure = "right")

breaks.sd <-classIntervals(ESA.values, n=5, style="sd", 
                           intervalClosure = "right")

breaks.equal <-classIntervals(ESA.values, n=5, style="equal", 
                              intervalClosure = "right")

## create maps of the data for each classification type
## standard deviation maps need one more break than color (i don't know why but
## throws an error otherwise)

ESA_quant <-image(ESA_total, col=viridis(5), breaks=breaks.quant$brks, 
                  axes=FALSE, xlab=NULL, ylab=NULL)
title(main="Total abundance of ESA-listed species, quantiles")

ESA_sd <-image(ESA_total, col=viridis(6), breaks=breaks.sd$brks, 
               axes=FALSE, xlab=NULL, ylab=NULL)
title(main="Total abundance of ESA-listed species, standard deviation")

ESA_equal <-image(ESA_total, col=viridis(5), breaks=breaks.equal$brks, 
                  axes=FALSE, xlab=NULL, ylab=NULL)
title(main="Total abundance of ESA-listed species, equal interval")


## manually set the color bins for each of the class breaks. based on the 
## viridis package's color ramp

col_quant <- findInterval(bins$breaks, breaks.quant$brks, all.inside=TRUE)
col_quant[which(col_quant==5)] <-"#ffcc33"
col_quant[which(col_quant==4)] <-"#66cc66"
col_quant[which(col_quant==3)] <-"#336666"
col_quant[which(col_quant==2)] <-"#333366"
col_quant[which(col_quant==1)] <-"#330033"


col_sd <- findInterval(bins$breaks, breaks.sd$brks, all.inside=TRUE)
col_sd[which(col_sd==5)] <-"#ffcc33"
col_sd[which(col_sd==4)] <-"#66cc66"
col_sd[which(col_sd==3)] <-"#336666"
col_sd[which(col_sd==2)] <-"#333366"
col_sd[which(col_sd==1)] <-"#330033"

col_equal <- findInterval(bins$breaks, breaks.equal$brks, all.inside=TRUE)
col_equal[which(col_equal==5)] <-"#ffcc33"
col_equal[which(col_equal==4)] <-"#66cc66"
col_equal[which(col_equal==3)] <-"#336666"
col_equal[which(col_equal==2)] <-"#333366"
col_equal[which(col_equal==1)] <-"#330033"


## plot histograms of the raster (100 bars), colored according to each of the 
## classification types, with no borders on the bars
## the x-axis will show the proper breakpoints to 2 decimal points

ESA_quant_hist <- hist(ESA_total, breaks=100, col=col_quant, xaxt="n", lty =
                         "blank", xlab ="quantiles", ylab = NULL, main = NULL)
axis(1, at=breaks.quant$brks, labels=round(breaks.quant$brks, digits = 2))

ESA_sd_hist <- hist(ESA_total, breaks=100, col=col_sd, xaxt="n", lty = "blank",
     xlab ="standard deviation", ylab = NULL, main = NULL)
axis(1, at=breaks.sd$brks, labels=round(breaks.sd$brks, digits = 2))

ESA_equal_hist <- hist(ESA_total, breaks=100, col=col_equal, xaxt="n", lty = 
                         "blank", xlab ="equal interval", ylab = NULL, main = NULL)
axis(1, at=breaks.equal$brks, labels=round(breaks.equal$brks, digits = 2))


## plot the maps and histograms in a single figure
par(mfrow=c(2,3))
ESA_quant <-image(ESA_total, col=viridis(5), breaks=breaks.quant$brks, 
                  axes=FALSE, xlab="", ylab="")
title(main="Quantiles \nTotal abundance of ESA-listed cetaceans")

ESA_sd <-image(ESA_total, col=viridis(6), breaks=breaks.sd$brks, 
               axes=FALSE, xlab="", ylab="")
title(main="Standard Deviation \nTotal abundance of ESA-listed cetaceans")

ESA_equal <-image(ESA_total, col=viridis(5), breaks=breaks.equal$brks, 
                  axes=FALSE, xlab="", ylab="")
title(main="Equal Interval \nTotal abundance of ESA-listed cetaceans")

ESA_quant_hist <- hist(ESA_total, breaks=100, col=col_quant, xaxt="n", lty =
                         "blank", xlab ="quantiles", ylab = NULL, main = NULL)
axis(1, at=breaks.quant$brks, labels=round(breaks.quant$brks, digits = 2))

ESA_sd_hist <- hist(ESA_total, breaks=100, col=col_sd, xaxt="n", lty = "blank",
                    xlab ="standard deviation", ylab = NULL, main = NULL)
axis(1, at=breaks.sd$brks, labels=round(breaks.sd$brks, digits = 2))

ESA_equal_hist <- hist(ESA_total, breaks=100, col=col_equal, xaxt="n", lty = 
                         "blank", xlab ="equal interval", ylab = NULL, main = NULL)
axis(1, at=breaks.equal$brks, labels=round(breaks.equal$brks, digits = 2))
