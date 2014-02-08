## googleVis demo
pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

## For the demo a web browser with internet connection and Flash is required.

df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))

## Line chart
Line <- gvisLineChart(df)
plot(Line)
pause()

## Line chart with two axis
Line2 <- gvisLineChart(df, "country", c("val1","val2"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                         vAxes="[{title:'val1'}, {title:'val2'}]"
                       ))
plot(Line2)
pause()

## Setting options, it works similar for other charts
Line3 <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
                        options=list(
                          title="Hello World",
                          titleTextStyle="{color:'red', 
                                           fontName:'Courier', 
                                           fontSize:16}",                         
                          backgroundColor="#D3D3D3",                          
                          vAxis="{gridlines:{color:'red', count:3}}",
                          hAxis="{title:'Country', titleTextStyle:{color:'blue'}}",
                          series="[{color:'green', targetAxisIndex: 0},	
                                   {color: 'orange',targetAxisIndex:1}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]",
                          legend="bottom",
                          curveType="function",
                          width=500,
                          height=300                         
                        ))
plot(Line3)
pause()

## Add edit button for on the fly customisation
## The same option is available for all other charts
Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
                        options=list(gvis.editor="Edit me!"))
plot(Line4)
pause()

## Bar chart
Bar <- gvisBarChart(df)
plot(Bar)
pause()

## Column chart
Column <- gvisColumnChart(df)
plot(Column)
pause()

## Area chart
Area <- gvisAreaChart(df)
plot(Area)
pause()

## Stepped Area chart
SteppedArea <- gvisSteppedAreaChart(df, xvar="country", 
                                    yvar=c("val1", "val2"),
                                    options=list(isStacked=TRUE))
plot(SteppedArea)
pause()

## Combo chart
Combo <- gvisComboChart(df, xvar="country",
                        yvar=c("val1", "val2"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}'))
plot(Combo)
pause()

## Scatter chart
Scatter <- gvisScatterChart(women, 
                            options=list(
                              legend="none",
                              lineWidth=2, pointSize=0,
                              title="Women", vAxis="{title:'weight (lbs)'}",
                              hAxis="{title:'height (in)'}", 
                              width=300, height=300))
plot(Scatter)
pause()

## Bubble chart
Bubble <- gvisBubbleChart(Fruits, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)
pause()

## Candlestick chart
Candle <- gvisCandlestickChart(OpenClose, 
                               options=list(legend='none'))
plot(Candle)
pause()

## Pie chart
Pie <- gvisPieChart(CityPopularity)
plot(Pie)
pause()

## Gauge
Gauge <-  gvisGauge(CityPopularity, 
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)
pause()

## Org chart
Org <- gvisOrgChart(Regions, 
                    options=list(width=600, height=250,
                                 size='large', allowCollapse=TRUE))
plot(Org)
pause()

## Intensity Map
Intensity <- gvisIntensityMap(df)
plot(Intensity)
pause()


## Geo Chart
Geo=gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)
pause()


## Example showing US data by state 
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

## Show Hurricane Andrew (1992) storm track with Geo Chart
GeoMarker <- gvisGeoChart(Andrew, "LatLong", 
                          sizevar='Speed_kt',
                          colorvar="Pressure_mb", 
                          options=list(region="US"))
plot(GeoMarker)
pause()

## Hurricane Andrew (1992) storm track with Google Maps
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)
pause()


## Table, click on the column header to sort the rows 
Table <- gvisTable(Exports, 
                   options=list(width=400, 
                                height=300))
plot(Table)
pause()

## Table with embedded links
PopTable <- gvisTable(Population, 
                      options=list(width=600, 
                                   height=300, 
                                   page='enable'))
plot(PopTable)
pause()

## Tree Map. Left mouse-click to drill down, right mouse-click to move up a hierarchy
Tree <- gvisTreeMap(Regions,  
                    "Region", "Parent", 
                    "Val", "Fac", 
                    options=list(fontSize=16))
plot(Tree)
pause()

## AnnotationChart does not require Flash
## Colouring the area below the lines to create an area chart
A4 <- gvisAnnotationChart(Stock, 
                          datevar="Date",
                          numvar="Value", 
                          idvar="Device",
                          titlevar="Title", 
                          annotationvar="Annotation",
                          options=list(
                            width=600, height=350,
                            fill=10, displayExactValues=TRUE,
                            colors="['#0000ff','#00ff00']")
)
plot(A4)
pause()

## Sankey chart
dat <- data.frame(From=c(rep("A",3), rep("B", 3)),
                  To=c(rep(c("X", "Y", "Z"),2)),
                  Weight=c(5,7,6,2,9,4))

sk2 <- gvisSankey(dat, from="From", to="To", weight="Weight",
                  options=list(
                    sankey="{link: {color: { fill: '#d799ae' } },
                            node: { color: { fill: '#a61d4c' },
                            label: { color: '#871b47' } }}"))
plot(sk2)
pause()

## Histogram
set.seed(123)
dat=data.frame(A=rpois(100, 20),
               B=rpois(100, 5),
               C=rpois(100, 50))
hist3 <- gvisHistogram(dat, options=list(
  legend="{ position: 'top', maxLines: 2 }",
  colors="['#5C3292', '#1A8763', '#871B47']"))

plot(hist3)
pause()

# Calendar chart
cl2 <- gvisCalendar(Cairo, 
                    datevar="Date", 
                    numvar="Temp",
                    options=list(
                      title="Daily temperature in Cairo",
                      height=500,
                      calendar="{yearLabel:{fontName: 'Times-Roman',
                                fontSize:32, color:'#1A8763', bold: true},
                                cellColor:{ stroke:'red', strokeOpacity:0.2},
                                focusedCellColor:{stroke:'red'}}"))
plot(cl2)
pause()

# Timeline chart
dat <- data.frame(Position=c(rep("President", 3), rep("Vice", 3)),
                  Name=c("Washington", "Adams", "Jefferson",
                         "Adams", "Jefferson", "Burr"),
                  start=as.Date(x=rep(c("1789-03-29", "1797-02-03", 
                                        "1801-02-03"),2)),
                  end=as.Date(x=rep(c("1797-02-03", "1801-02-03", 
                                      "1809-02-03"),2)))

tl <- gvisTimeline(data=dat, 
                   rowlabel="Name",
                   barlabel="Position",
                   start="start", 
                   end="end",
                   options=list(timeline="{groupByRowLabel:false}",
                                backgroundColor='#ffd', 
                                height=350,
                                colors="['#cbb69d', '#603913', '#c69c6e']"))
plot(tl)
pause()

## Flash charts

## Geo Map, requires Flash
Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit",
               options=list(height=350, dataMode='regions'))
plot(Geo)
pause()


## Show Hurricane Andrew (1992) storm track with Geo Map
AndrewGeo <- gvisGeoMap(Andrew, 
                        locationvar="LatLong", 
                        numvar="Speed_kt", 
                        hovervar="Category", 
                        options=list(height=350, 
                                     region="US", 
                                     dataMode="markers"))
plot(AndrewGeo)
pause()

## Annotated time line chart, requires Flash
AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, 
                                       datevar="Date",
                                       numvar="Value", 
                                       idvar="Device",
                                       titlevar="Title", 
                                       annotationvar="Annotation",
                                       options=list(displayAnnotations=TRUE,
                                                    width=600, height=350))
plot(AnnoTimeLine)
pause()

## Motion chart, requires Flash
Motion=gvisMotionChart(Fruits, 
                       idvar="Fruit", 
                       timevar="Year")
plot(Motion)
pause()

## You can change some of displaying settings via the browser,
## e.g. the level of opacity of non-selected items, or the chart type.
## The state string from the 'Advanced' tab can be used to set those
## settings via R. Just copy and past the string from the browser into
## the argument state of the options list.
## Here is an example of a motion chart, with an initial line chart
## displayed. 
myStateSettings <-'
{"xZoomedDataMin":1199145600000,"colorOption":"2",
"duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
"yAxisOption":"4","sizeOption":"_UNISIZE",
"iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
"xZoomedDataMax":1262304000000,"iconType":"LINE",
"dimensions":{"iconDimensions":["dim0"]},
"showTrails":false,"uniColorForNonSelected":false,
"xAxisOption":"_TIME","orderedByX":false,"playDuration":15000,
"xZoomedIn":false,"time":"2010","yZoomedDataMin":0,
"yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}
'
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(state=myStateSettings))
plot(M)


## gvisMerge: multiple charts on one page
G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=220, height=100))
T <- gvisTable(Exports, 
                  options=list(width=220, height=260))

GT <- gvisMerge(G,T, horizontal=FALSE) 
plot(GT)
pause()

M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=440, height=360))
GTM <- gvisMerge(GT, M, horizontal=TRUE,
                 tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
plot(GTM)
pause()

## See demo(package='googleVis') for other available demos.
