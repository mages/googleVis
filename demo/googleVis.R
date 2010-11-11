## googleVis demo
## A web browser with internet connection and Flash is required

## See how googleVis functions can be integrated into rsp-files:
browseRsp("http://127.0.0.1:8074/library/googleVis/rsp/")

## Motion Chart
Motion=gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(Motion)

## Geo Map
Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit")
plot(Geo)

## Map
Andrew$LatLong=with(Andrew, paste(Lat, Long, sep=":"))
Andrew$Tip=with(Andrew,paste(Category, "<BR>Presure=",Presure_mb, "<BR>Speed=", Speed_kt, sep=""))
Map <- gvisMap(Andrew, "LatLong" , "Tip", 
      	      options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
		      mapType='terrain', useMapTypeControl=TRUE))
plot(Map)

## Table 
Table=gvisTable(Exports, options=list(width=400, height=300))
plot(Table)

## TreeMap
Tree=gvisTreeMap(Regions,  "Region", "Parent", "Val", "Fac", options=list(fontSize=16))
plot(Tree)


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
                 			Chart4=Map$html$chart,
					Caption4=Map$html$caption,
					Footer=Tree$html$footer)
            )
		

class(Page) <- list("gvis", class(Page))
plot(Page)
