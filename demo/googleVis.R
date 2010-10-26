## googleVis demo
## A web browse with internet connection and Flash is required

## See ahow googleVis functions can be integrated into rsp-files:
browseRsp("http://127.0.0.1:8074/library/googleVis/rsp/")

## Motion Chart
Motion=gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(Motion)

## Geo Map
Geo=gvisGeoMap(Exports[,1:2], idvar="Country")
plot(Geo)

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
					Footer=Tree$html$footer)
			)
		

class(Page) <- list("gvis", class(Page))
plot(Page)
