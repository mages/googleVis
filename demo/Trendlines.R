## Trend line demo
# A trendline is a line superimposed on a chart revealing the overall direction 
# of the data. Google Charts can automatically generate trendlines for 
# Scatter Charts, Bar Charts, Column Charts, and Line Charts.
# 
# Fore more details see:
# https://developers.google.com/chart/interactive/docs/gallery/trendlines

## Linear trend line

## Add a trend line to the first series
lt <- gvisScatterChart(women, options=list(trendlines="0" ))
plot(lt)

## Exponential trend line and show equation in legend
et <- gvisScatterChart(women, options=list(
   trendlines="{0: { type: 'exponential',  
                     visibleInLegend: 'true', 
                     color: 'green',
                     lineWidth: 10,
                     opacity: 0.5}}"))
plot(et)

dat <- data.frame(week=1:6,
                  #country=c("US", "GB", "BR"),
                  val1=c(1,3,4,5,6,8), 
                  val2=c(12,23,32,40,50,55))
## Column chart with two trend lines
## The first column has to be ignored for trendlines
Col1 <- gvisColumnChart(dat[,2:3],
                        options=list(trendlines="{0: {}, 1:{}}"))
plot(Col1)

# Give the trendlines different labels
Col2 <- gvisColumnChart(dat[,2:3],
                        options=list(trendlines="{
                          0: {
                            labelInLegend: 'Trendline 1',
                            visibleInLegend: true,}, 
                          1:{
                            labelInLegend: 'Trendline 2',
                            visibleInLegend: true}
                          }"
                        ))
plot(Col2)
