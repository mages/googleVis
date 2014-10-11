## This demo shows how country level data can be accessed from
## the World Bank via their API and displayed with a Motion Chart.
## Inspired by Google's Public Data Explorer, see
## http://www.google.com/publicdata/home
##
## For the World Bank Data terms of use see:
## http://data.worldbank.org/summary-terms-of-use
##
## To run this demo an internet connection and Flash are required.
## This demo is part of the googleVis R package.
##
## See also: http://lamages.blogspot.com/2011/09/accessing-and-plotting-world-bank-data.html
## Markus Gesmann, 24 September 2011
##
## Thanks to John Maindonald for a simplified version of this
## demo using the WDI package.
##
## Distributed under GPL 2 or later

## This demo requires the 'WDI' package
if( !is.element("WDI", installed.packages()[,1]) )
  install.packages("WDI")

library(WDI)
inds <- c('SP.DYN.TFRT.IN','SP.DYN.LE00.IN', 'SP.POP.TOTL',
          'NY.GDP.PCAP.CD', 'SE.ADT.1524.LT.FE.ZS')
indnams <- c("fertility.rate", "life.expectancy", "population",
             "GDP.per.capita.Current.USD", "15.to.25.yr.female.literacy")
wdiData <- WDI(country="all", indicator=inds,
               start=1960, end=format(Sys.Date(), "%Y"), extra=TRUE)
colnum <- match(inds, names(wdiData))

names(wdiData)[colnum] <- indnams
## Create a motion chart
library(googleVis)
WorldBank <- droplevels(subset(wdiData, !region %in% "Aggregates"))
M <- gvisMotionChart(WorldBank,
                     idvar="country", timevar="year",
                     xvar="life.expectancy", yvar="fertility.rate",
                     colorvar="region", sizevar="population",
                     options=list(width=700, height=600),
                     chartid="WorldBank")
## Display the chart in the browser
plot(M)
