## googleVis demo

pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

## For the demo a web browser with internet connection and Flash is required.
## Further you need write access to the following directory
system.file(file.path("rsp", "myAnalysis"), package="googleVis")

## Motion Chart
Motion=gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(Motion)
pause()

## Geo Map with coloured regions
Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit",
                       options=list(height=350, dataMode='regions'))
plot(Geo)
pause()


## Show Hurricane Andrew (1992) storm track with Geo Map
AndrewGeo <- gvisGeoMap(Andrew, locationvar="LatLong", numvar="Speed_kt", 
      			 hovervar="Category", 
      		         options=list(height=350, region="US"))

plot(AndrewGeo)
pause()

## Hurricane Andrew (1992) storm track with Google Maps
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
      	      options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
		      mapType='terrain', useMapTypeControl=TRUE))
plot(AndrewMap)
pause()


## Table. Click on the column header to sort the rows 
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

## Annotated Time Line Chart
AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                           numvar="Value", idvar="Device",
                           titlevar="Title", annotationvar="Annotation",
                           options=list(displayAnnotations=TRUE,
                            width=600, height=350)
                           )
plot(AnnoTimeLine)
pause()


## Several charts on one page
Page <- list(type="MotionGeoTableTree", 
             chartid=format(Sys.time(), "MotionGeoTableTree-%Y-%m-%d-%H-%M-%S"), 
             html=list(header=Motion$html$header,
               chart=list(Motion$html$chart,
                 Geo$html$chart, 
                 Table$html$chart,
                 Tree$html$chart, 
                 AndrewMap$html$chart, 
                 AnnoTimeLine$html$chart),
               footer=Tree$html$footer)
             )
		

class(Page) <- list("gvis", class(Page))
plot(Page)
pause()


## See how googleVis functions can be integrated into rsp-files:
if(require(R.rsp))
  browseRsp() ## Click on googleVis in the Package section.
