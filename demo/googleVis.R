## googleVis demo

pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

## A web browser with internet connection and Flash is required

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


## Table 
Table=gvisTable(Exports, options=list(width=400, height=300))
plot(Table)
pause()

## Table with embed links
PopTable=gvisTable(Population, options=list(width=600, height=300, page='enable'))
plot(PopTable)
pause()

## TreeMap
Tree=gvisTreeMap(Regions,  "Region", "Parent", "Val", "Fac", options=list(fontSize=16))
plot(Tree)
pause()



## Several charts on one page
Page <- list(type="MotionGeoTableTree", 
			 chartid=format(Sys.time(), "MotionGeoTableTree-%Y-%m-%d-%H-%M-%S"), 
			 html=list(Header=Motion$html$header,
			 		Chart1=Motion$html$chart,
					Caption1=Motion$html$caption,
					Chart2=Geo$html$chart,
					Caption2=Geo$html$caption,
					Chart3=Table$html$chart,
					Caption3=Table$html$caption,
					Chart4=Tree$html$chart,
					Caption4=Tree$html$caption,
                 			Chart4=AndrewMap$html$chart,
					Caption4=AndrewMap$html$caption,
					Footer=Tree$html$footer)
            )
		

class(Page) <- list("gvis", class(Page))
plot(Page)
pause()


## See how googleVis functions can be integrated into rsp-files:
browseRsp("http://127.0.0.1:8074/library/googleVis/rsp/")
