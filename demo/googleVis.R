## ---- googleVis demo ----
## ---- pauseFunction ----
pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

## ---- testData ----
df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))


## ---- LineChart ----
Line <- gvisLineChart(df)
plot(Line)

## ---- pause ----
pause()

## ---- TwoAxis ----
Line2 <- gvisLineChart(df, "country", c("val1","val2"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                         vAxes="[{title:'val1'}, {title:'val2'}]"
                       ))
plot(Line2)

## ---- pause ----
pause()

## ---- SettingOptions ----
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

## ---- pause ----
pause()

## ---- CustomizingLines ----
Dashed <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
                        options=list(
                          series="[{color:'green', targetAxisIndex: 0, 
                          lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                          {color: 'blue',targetAxisIndex: 1, 
                          lineWidth: 2, lineDashStyle: [4, 1]}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]"
                        ))
plot(Dashed)

## ---- pause ----
pause()

## ---- EditButton ----
Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
                        options=list(gvis.editor="Edit me!"))
plot(Line4)



## ---- pause ----
pause()

## ---- BarChart ----
Bar <- gvisBarChart(df)
plot(Bar)

## ---- pause ----
pause()

## ---- ColumnChart ----
Column <- gvisColumnChart(df)
plot(Column)

## ---- pause ----
pause()

## ---- AreaChart ----
Area <- gvisAreaChart(df)
plot(Area)

## ---- pause ----
pause()

## ---- SteppedAreaChart ----
SteppedArea <- gvisSteppedAreaChart(df, xvar="country", 
                                    yvar=c("val1", "val2"),
                                    options=list(isStacked=TRUE))
plot(SteppedArea)

## ---- pause ----
pause()

## ---- ComboChart ----
Combo <- gvisComboChart(df, xvar="country",
                        yvar=c("val1", "val2"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}'))
plot(Combo)

## ---- pause ----
pause()

## ---- ScatterChart ----
Scatter <- gvisScatterChart(women, 
                            options=list(
                              legend="none",
                              lineWidth=2, pointSize=0,
                              title="Women", vAxis="{title:'weight (lbs)'}",
                              hAxis="{title:'height (in)'}", 
                              width=300, height=300))
plot(Scatter)

## ---- ScatterChartPoints ----
M <- matrix(nrow=6,ncol=6)
M[col(M)==row(M)] <- 1:6
dat <- data.frame(X=1:6, M)
SC <- gvisScatterChart(dat, 
                       options=list(
                         title="Customizing points",
                         legend="right",
                         pointSize=30,
                         series="{
                              0: { pointShape: 'circle' },
                              1: { pointShape: 'triangle' },
                              2: { pointShape: 'square' },
                              3: { pointShape: 'diamond' },
                              4: { pointShape: 'star' },
                              5: { pointShape: 'polygon' }
                              }"))
plot(SC)

## ---- pause ----
pause()

## ---- BubbleChart ----
Bubble <- gvisBubbleChart(Fruits, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)

## ---- pause ----
pause()

## ---- CandlestickChart ----
Candle <- gvisCandlestickChart(OpenClose, 
                               options=list(legend='none'))
plot(Candle)

## ---- pause ----
pause()

## ---- PieChart ----
Pie <- gvisPieChart(CityPopularity)
plot(Pie)

## ---- pause ----
pause()

## ---- Gauge ----
Gauge <-  gvisGauge(CityPopularity, 
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)

## ---- pause ----
pause()

## ---- IntensityMap ----
Intensity <- gvisIntensityMap(df)
plot(Intensity)

## ---- pause ----
pause()


## ---- GeoChart ----
Geo=gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

## ---- pause ----
pause()


## ---- USStateData ----
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

## ---- pause ----
pause()

## ---- GeoChartHurricaneAndrew ----
GeoMarker <- gvisGeoChart(Andrew, "LatLong", 
                          sizevar='Speed_kt',
                          colorvar="Pressure_mb", 
                          options=list(region="US"))
plot(GeoMarker)

## ---- pause ----
pause()

## ---- GoogleMapHurricaneAndrew ----
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)

## ---- pause ----
pause()


## ---- Table ----
Table <- gvisTable(Stock, 
                   formats=list(Value="#,###"))
plot(Table)

## ---- pause ----
pause()

## ---- TableWithPages ----
PopTable <- gvisTable(Population, 
                      formats=list(Population="#,###",
                                   '% of World Population'='#.#%'),
                      options=list(page='enable'))
plot(PopTable)

## ---- pause ----
pause()

## ---- OrgChart ----
Org <- gvisOrgChart(Regions, 
                    options=list(width=600, height=250,
                                 size='large', allowCollapse=TRUE))
plot(Org)

## ---- pause ----
pause()


## ---- TreeMap ----
Tree <- gvisTreeMap(Regions,  
                    "Region", "Parent", 
                    "Val", "Fac", 
                    options=list(fontSize=16))
plot(Tree)

## ---- pause ----
pause()

## ---- AnnotationChart ----
Anno <- gvisAnnotationChart(Stock, 
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
plot(Anno)

## ---- pause ----
pause()

## ---- SankeyChart ----
datSK <- data.frame(From=c(rep("A",3), rep("B", 3)),
                    To=c(rep(c("X", "Y", "Z"),2)),
                    Weight=c(5,7,6,2,9,4))

Sankey <- gvisSankey(datSK, from="From", to="To", weight="Weight",
                     options=list(
                       sankey="{link: {color: { fill: '#d799ae' } },
                            node: { color: { fill: '#a61d4c' },
                            label: { color: '#871b47' } }}"))
plot(Sankey)

## ---- pause ----
pause()

# ---- CalendarChart ----
Cal <- gvisCalendar(Cairo, 
                    datevar="Date", 
                    numvar="Temp",
                    options=list(
                      title="Daily temperature in Cairo",
                      height=320,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
)
plot(Cal)

## ---- pause ----
pause()

# ---- Timeline ----
datTL <- data.frame(Position=c(rep("President", 3), rep("Vice", 3)),
                    Name=c("Washington", "Adams", "Jefferson",
                           "Adams", "Jefferson", "Burr"),
                    start=as.Date(x=rep(c("1789-03-29", "1797-02-03", 
                                          "1801-02-03"),2)),
                    end=as.Date(x=rep(c("1797-02-03", "1801-02-03", 
                                        "1809-02-03"),2)))

Timeline <- gvisTimeline(data=datTL, 
                         rowlabel="Name",
                         barlabel="Position",
                         start="start", 
                         end="end",
                         options=list(timeline="{groupByRowLabel:false}",
                                      backgroundColor='#ffd', 
                                      height=350,
                                      colors="['#cbb69d', '#603913', '#c69c6e']"))
plot(Timeline)

## ---- pause ----
pause()

## ---- Histogram ----
set.seed(123)
datHist=data.frame(A=rpois(100, 20),
                   B=rpois(100, 5),
                   C=rpois(100, 50))

Hist <- gvisHistogram(datHist, options=list(
  legend="{ position: 'top', maxLines: 2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=400, height=360))
plot(Hist)

## ---- pause ----
pause()

## ---- gvisMerge ----
G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=300, height=300))
T <- gvisTable(Exports, 
               options=list(width=220, height=300))

GT <- gvisMerge(G,T, horizontal=TRUE) 
plot(GT)

## ---- pause ----
pause()


## Flash charts
##  ---- GeoMap ----
Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit",
               options=list(height=350, dataMode='regions'))
plot(Geo)

## ---- pause ----
pause()


## ---- GeoMap ----
AndrewGeo <- gvisGeoMap(Andrew, 
                        locationvar="LatLong", 
                        numvar="Speed_kt", 
                        hovervar="Category", 
                        options=list(height=350, 
                                     region="US", 
                                     dataMode="markers"))
plot(AndrewGeo)

## ---- pause ----
pause()

## ---- AnnotatedTimeLine ----
AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, 
                                       datevar="Date",
                                       numvar="Value", 
                                       idvar="Device",
                                       titlevar="Title", 
                                       annotationvar="Annotation",
                                       options=list(displayAnnotations=TRUE,
                                                    width="600px", height="350px"))
plot(AnnoTimeLine)

## ---- pause ----
pause()

## ---- MotionChart ----
Motion=gvisMotionChart(Fruits, 
                       idvar="Fruit", 
                       timevar="Year")
plot(Motion)

## ---- pause ----
pause()


## You can change some of displaying settings via the browser,
## e.g. the level of opacity of non-selected items, or the chart type.
## The state string from the 'Advanced' tab can be used to set those
## settings via R. Just copy and past the string from the browser into
## the argument state of the options list.
## Here is an example of a motion chart, with an initial line chart
## displayed.

## ---- MotionChartSettings ----
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


## See demo(package='googleVis') for other available demos.
