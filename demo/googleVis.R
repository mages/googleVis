## googleVis demo
## A web browse with internet connection and Flash is required

## See ahow googleVis functions can be integrated into rsp-files:
browseRsp("http://127.0.0.1:8074/library/googleVis/rsp/")

## Motion Chart
Motion=gvisMotionChart(Fruits, "Fruit", "Year")
plot(Motion)

## Geo Map
Geo=gvisGeoMap(Exports[,1:2])
plot(Geo)

## Table 
Table=gvisTable(Exports, options=list(width=400, height=300))
plot(Table)

## TreeMap
Tree=gvisTreeMap(Regions,  "Region", "parent", "Val", "Fac", options=list(fontSize=16))
plot(Tree)

## Several charts on one page
Page <- list(type="MotionGeoTableTree", 
			 chartid=format(Sys.time(), "MotionGeoTableTree-%Y-%m-%d-%H-%M-%S"), 
			 html=list(Header=Motion$html$Header,
			 		Chart1=Motion$html$Chart,
					Caption1=Motion$html$Caption,
					Chart2=Geo$html$Chart,
					Caption2=Geo$html$Caption,
					Chart3=Table$html$Chart,
					Caption3=Table$html$Caption,
					Chart4=Tree$html$Chart,
					Caption4=Tree$html$Caption,
					Footer=Tree$html$Footer)
			)
		

class(Page) <- list("gvis", class(Page))
plot(Page)
