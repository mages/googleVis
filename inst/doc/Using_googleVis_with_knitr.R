
## ----setOptions, message=FALSE-------------------------------------------
library(googleVis)
op <- options(gvis.plot.tag='chart')


## ----ComboExample, results='asis', tidy=FALSE----------------------------
## Add the mean
CityPopularity$Mean=mean(CityPopularity$Popularity)
CC <- gvisComboChart(CityPopularity, xvar='City',
          yvar=c('Mean', 'Popularity'),
          options=list(seriesType='bars',
                       width=450, height=300,
                       title='City Popularity',
                       series='{0: {type:\"line\"}}'))
plot(CC)


## ----gvisMergeExample, results='asis', echo=FALSE------------------------
Geo <- gvisGeoChart(Exports, locationvar='Country', colorvar='Profit', 
                    options=list(height=300, width=350)) 
Tbl <- gvisTable(Exports, options=list(height=300, width=200))
plot(gvisMerge(Geo, Tbl, horizontal=TRUE))


## ----MotionChartExample, results='asis', tidy=FALSE----------------------
M <- gvisMotionChart(Fruits, 'Fruit', 'Year',
         options=list(width=400, height=350))
plot(M)


## ----resetOptions--------------------------------------------------------
## Set options back to original options
options(op)


