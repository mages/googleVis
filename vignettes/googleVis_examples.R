## ----demo, eval=FALSE---------------------------------------------------------
#  library(googleVis)
#  demo(googleVis)

## ----setOptions, message=FALSE, echo=FALSE------------------------------------
library(googleVis)
library(knitr)
op <- options(gvis.plot.tag='chart')
read_demo('googleVis', 'googleVis')

## ----testData, tidy=FALSE-----------------------------------------------------
df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))

## ----LineChart, results='asis', tidy=FALSE------------------------------------
Line <- gvisLineChart(df)
plot(Line)

## ----TwoAxis, results='asis', tidy=FALSE--------------------------------------
Line2 <- gvisLineChart(df, "country", c("val1","val2"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                         vAxes="[{title:'val1'}, {title:'val2'}]"
                       ))
plot(Line2)

## ----BarChart, results='asis', tidy=FALSE-------------------------------------
Bar <- gvisBarChart(df)
plot(Bar)

## ----ColumnChart, results='asis', tidy=FALSE----------------------------------
Column <- gvisColumnChart(df)
plot(Column)

## ----AreaChart, results='asis', tidy=FALSE------------------------------------
Area <- gvisAreaChart(df)
plot(Area)

## ----SteppedAreaChart, results='asis', tidy=FALSE-----------------------------
SteppedArea <- gvisSteppedAreaChart(df, xvar="country", 
                                    yvar=c("val1", "val2"),
                                    options=list(isStacked=TRUE))
plot(SteppedArea)

## ----ComboChart, results='asis', tidy=FALSE-----------------------------------
Combo <- gvisComboChart(df, xvar="country",
                        yvar=c("val1", "val2"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}'))
plot(Combo)

## ----ScatterChart, results='asis', tidy=FALSE---------------------------------
Scatter <- gvisScatterChart(women, 
                            options=list(
                              legend="none",
                              lineWidth=2, pointSize=0,
                              title="Women", vAxis="{title:'weight (lbs)'}",
                              hAxis="{title:'height (in)'}", 
                              width=300, height=300))
plot(Scatter)

## ----BubbleChart, results='asis', tidy=FALSE----------------------------------
Bubble <- gvisBubbleChart(Fruits, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)

## ----CustomizingLines, results='asis', tidy=FALSE-----------------------------
Dashed <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
                        options=list(
                          series="[{color:'green', targetAxisIndex: 0, 
                          lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                          {color: 'blue',targetAxisIndex: 1, 
                          lineWidth: 2, lineDashStyle: [4, 1]}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]"
                        ))
plot(Dashed)

## ----ScatterChartPoints, results='asis', tidy=FALSE---------------------------
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

## ----EditButton, results='asis', tidy=FALSE-----------------------------------
Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
                        options=list(gvis.editor="Edit me!"))
plot(Line4)

## ----SettingOptions, results='asis', tidy=FALSE-------------------------------
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

## ----CandlestickChart, results='asis', tidy=FALSE-----------------------------
Candle <- gvisCandlestickChart(OpenClose, 
                               options=list(legend='none'))
plot(Candle)

## ----PieChart, results='asis', tidy=FALSE-------------------------------------
Pie <- gvisPieChart(CityPopularity)
plot(Pie)

## ----Gauge, results='asis', tidy=FALSE----------------------------------------
Gauge <-  gvisGauge(CityPopularity, 
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)

## ----GeoChart, results='asis', tidy=FALSE-------------------------------------
Geo=gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

## ----USStateData, results='asis', tidy=FALSE----------------------------------
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

## ----GeoChartHurricaneAndrew, results='asis', tidy=FALSE----------------------
GeoMarker <- gvisGeoChart(Andrew, "LatLong", 
                          sizevar='Speed_kt',
                          colorvar="Pressure_mb", 
                          options=list(region="US"))
plot(GeoMarker)

## ----Table, results='asis', tidy=FALSE----------------------------------------
Table <- gvisTable(Stock, 
                   formats=list(Value="#,###"))
plot(Table)

## ----TableWithPages, results='asis', tidy=FALSE-------------------------------
PopTable <- gvisTable(Population, 
                      formats=list(Population="#,###",
                                   '% of World Population'='#.#%'),
                      options=list(page='enable'))
plot(PopTable)

## ----OrgChart, results='asis', tidy=FALSE-------------------------------------
Org <- gvisOrgChart(Regions, 
                    options=list(width=600, height=250,
                                 size='large', allowCollapse=TRUE))
plot(Org)

## ----TreeMap, results='asis', tidy=FALSE--------------------------------------
Tree <- gvisTreeMap(Regions,  
                    "Region", "Parent", 
                    "Val", "Fac", 
                    options=list(fontSize=16))
plot(Tree)

## ----AnnotationChart, results='asis', tidy=FALSE------------------------------
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

## ----SankeyChart, results='asis', tidy=FALSE----------------------------------
datSK <- data.frame(From=c(rep("A",3), rep("B", 3)),
                    To=c(rep(c("X", "Y", "Z"),2)),
                    Weight=c(5,7,6,2,9,4))

Sankey <- gvisSankey(datSK, from="From", to="To", weight="Weight",
                     options=list(
                       sankey="{link: {color: { fill: '#d799ae' } },
                            node: { color: { fill: '#a61d4c' },
                            label: { color: '#871b47' } }}"))
plot(Sankey)

## ----Histogram, results='asis', tidy=FALSE------------------------------------
set.seed(123)
datHist=data.frame(A=rpois(100, 20),
                   B=rpois(100, 5),
                   C=rpois(100, 50))

Hist <- gvisHistogram(datHist, options=list(
  legend="{ position: 'top', maxLines: 2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=400, height=360))
plot(Hist)

## ----CalendarChart, results='asis', tidy=FALSE--------------------------------
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

## ----Timeline, results='asis', tidy=FALSE-------------------------------------
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

## ----Gantt, results='asis', tidy=FALSE----------------------------------------
daysToMilliseconds <- function(days){
  days * 24 * 60 * 60 * 1000
}

dat <- data.frame(
  taskID = c("Research", "Write", "Cite", "Complete", "Outline"),
  taskName = c("Find sources", "Write Paper",  "Create bibliography", "Hand in paper", "Outline paper"),
  resource = c(NA, "write", "write", "complete", "write"),
  start = c(as.Date("2015-01-01"), NA, NA, NA, NA),
  end = as.Date(c("2015-01-05", "2015-01-09", "2015-01-07", "2015-01-10", "2015-01-06")),
  duration = c(NA, daysToMilliseconds(c(3, 1, 1, 1))),
  percentComplete = c(100, 25, 20, 0, 100),
  dependencies = c(NA, "Research, Outline", "Research", "Cite, Write", "Research")
)

gntt <- gvisGantt(dat, taskID = "taskID",
                  taskName = "taskName", 
                  resource = "resource",
                  start = "start",
                  end = "end", 
                  duration = "duration",
                  percentComplete = "percentComplete",
                  dependencies = "dependencies")

plot(gntt)

## ----WordTree, results='asis', tidy=FALSE-------------------------------------
wt1 <- gvisWordTree(Cats, textvar = "Phrase")
plot(wt1)

## ----gvisMerge, results='asis', tidy=FALSE------------------------------------
G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=300, height=300))
T <- gvisTable(Exports, 
               options=list(width=220, height=300))

GT <- gvisMerge(G,T, horizontal=TRUE) 
plot(GT)

