## googleVis demo
pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

## For the demo a web browser with internet connection and Flash is required.

df=data.frame(country=c("US", "GB", "BR"), val1=c(10,13,14), val2=c(23,12,32))

## Line chart
Line <- gvisLineChart(df)
plot(Line)
pause()

## Line chart with two axis
Line2 <- gvisLineChart(df, "country", c("val1","val2"),
                        options=list(series="[{targetAxisIndex: 0},
                                                    {targetAxisIndex:1}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]"
                          ))
plot(Line2)
pause()

## Setting options, it works similar for other charts
Line3 <-  gvisLineChart(df, "country", c("val1","val2"),
                        options=list(
                          title="Hello World",
                          titleTextStyle="{color:'red', fontName:'Courier', 
                                                 fontSize:16}",                         
                          backgroundColor="#D3D3D3",                          
                          vAxis="{gridlineColor:'#FFFFFF'}",
                          hAxis="{title:'Country', titleTextStyle:{color:'blue'}}",
                          series="[{targetAxisIndex: 0},
                                       {targetAxisIndex:1}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]",
                          legend="bottom",
                          curveType='function',
                          width=500,
                          height=300                         
                          ))
plot(Line3)

## Add edit button for on the fly customisation
## The same option is available for all other charts
Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
                        options=list(gvis.editor="Edit me!"))
plot(Line4)

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
SteppedArea <- gvisSteppedAreaChart(df, xvar="country", yvar=c("val1", "val2"),
      options=list(isStacked=TRUE))
plot(SteppedArea)

## Combo chart
Combo <- gvisComboChart(df, xvar="country",
                                     yvar=c("val1", "val2"),
                                   options=list(seriesType="bars",
                                                series='{1: {type:"line"}}'))
plot(Combo)
pause()

## Scatter chart
Scatter <- gvisScatterChart(women, options=list(legend="none",
                 lineWidth=2, pointSize=0,
                 title="Women", vAxis="{title:'weight (lbs)'}",
                 hAxis="{title:'height (in)'}", width=300, height=300))
                 
plot(Scatter)
pause()

## Candlestick chart
Candle <- gvisCandlestickChart(OpenClose, options=list(legend='none'))
plot(Candle)
pause()

## Pie chart
Pie <- gvisPieChart(CityPopularity)
plot(Pie)
pause()

## Gauge
Gauge <-  gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500,
                                      greenTo=800, yellowFrom=300, yellowTo=500,
                                      redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)
pause()

## Org chart
Org <- gvisOrgChart(Regions, options=list(width=600, height=250,
                               size='large', allowCollapse=TRUE))
plot(Org)
pause()

## Motion chart, requires Flash
Motion=gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
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


## Intensity Map
Intensity <- gvisIntensityMap(df)
plot(Intensity)
pause()


## Geo Chart
Geo=gvisGeoChart(Exports, locationvar="Country", colorvar="Profit")
plot(Geo)
pause()


## Example showing US data by state 
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                 options=list(region="US", displayMode="regions", resolution="provinces",
 		 width=600, height=400))
plot(GeoStates)

## Show Hurricane Andrew (1992) storm track with Geo Chart
GeoMarker <- gvisGeoChart(Andrew, "LatLong", sizevar='Speed_kt',
                   colorvar="Pressure_mb", options=list(region="US"))
plot(GeoMarker)
pause()

## Geo Map, requires Flash
Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit",
                       options=list(height=350, dataMode='regions'))
plot(Geo)
pause()


## Show Hurricane Andrew (1992) storm track with Geo Map
AndrewGeo <- gvisGeoMap(Andrew, locationvar="LatLong", numvar="Speed_kt", 
      			 hovervar="Category", 
      		         options=list(height=350, region="US", dataMode="markers"))

plot(AndrewGeo)
pause()

## Hurricane Andrew (1992) storm track with Google Maps
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
      	      options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
		      mapType='terrain', useMapTypeControl=TRUE))
plot(AndrewMap)
pause()


## Table, click on the column header to sort the rows 
Table <- gvisTable(Exports, options=list(width=400, height=300))
plot(Table)
pause()

## Table with embedded links
PopTable <- gvisTable(Population, options=list(width=600, height=300, page='enable'))
plot(PopTable)
pause()

## Tree Map. Left mouse-click to drill down, right mouse-click to move up a hierarchy
Tree <- gvisTreeMap(Regions,  "Region", "Parent", "Val", "Fac", options=list(fontSize=16))
plot(Tree)
pause()

## Annotated time line chart, requires Flash
AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                           numvar="Value", idvar="Device",
                           titlevar="Title", annotationvar="Annotation",
                           options=list(displayAnnotations=TRUE,
                            width=600, height=350)
                           )
plot(AnnoTimeLine)
pause()


## gvisMerge: multiple charts on one page
G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=200, height=100))
T <- gvisTable(Exports, 
                  options=list(width=200, height=260))

GT <- gvisMerge(G,T, horizontal=FALSE) 
plot(GT)
pause()

M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=400, height=360))
GTM <- gvisMerge(GT, M, horizontal=TRUE,
                 tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
plot(GTM)

pause()

## See how googleVis functions can be integrated into rsp-files:
if(require(R.rsp))
  browseRsp() ## Click on googleVis in the Package section.

## See demo(package='googleVis') for other available demos.
