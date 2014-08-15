## Trend line demo
# A trendline is a line superimposed on a chart revealing the overall direction 
# of the data. Google Charts can automatically generate trendlines for 
# Scatter Charts, Bar Charts, Column Charts, and Line Charts.
# 
# Fore more details see:
# https://developers.google.com/chart/interactive/docs/gallery/trendlines

## Linear trend line

## Add a trend line to the first series
## ---- LinearTrend ----
plot(
  gvisScatterChart(women, options=list(trendlines="0"))
)

## ---- ExponentialTrend ----
plot(
  gvisScatterChart(women, options=list(
    trendlines="{0: { type: 'exponential',  
                     visibleInLegend: 'true', 
                     color: 'green',
                     lineWidth: 10,
                     opacity: 0.5}}",
    chartArea="{left:50,top:20,width:'50%',height:'75%'}"))
)

## ---- ColumnChartWithTrendline ----
dat <- data.frame(val1=c(1,3,4,5,6,8), 
                  val2=c(12,23,32,40,50,55))
plot(
  gvisColumnChart(dat,
                  options=list(trendlines="{0: {}}"))
)

## ---- DifferentLabels ----
dat$val3 <- c(5,6,10,12,15,20)
plot(
  gvisColumnChart(dat,
                  options=list(trendlines="{
                          0: {
                            labelInLegend: 'Trendline 1',
                            visibleInLegend: true,}, 
                          1:{
                            labelInLegend: 'Trendline 2',
                            visibleInLegend: true}
                          }",
                          chartArea="{left:50,top:20,
                                      width:'50%',height:'75%'}"
                  ))
)

