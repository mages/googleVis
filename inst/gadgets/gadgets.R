Motion=gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
cat(createGoogleGadget(Motion), file="motionchart.xml")

## Geo Map with coloured regions
Geo=gvisGeoMap(Exports, locationvar="Country", numvar="Profit",
                       options=list(height=350, dataMode='regions'))
cat(createGoogleGadget(Geo), file="geomap.xml")


## Show Hurricane Andrew (1992) storm track with Geo Map
AndrewGeo <- gvisGeoMap(Andrew, locationvar="LatLong", numvar="Speed_kt", 
      			 hovervar="Category", 
      		         options=list(height=350, region="US"))

cat(createGoogleGadget(AndrewGeo), file="andrewgeomap.xml")

## Hurricane Andrew (1992) storm track with Google Maps
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
      	      options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
		      mapType='terrain', useMapTypeControl=TRUE))

cat(createGoogleGadget(AndrewMap), file="andrewmap.xml")


## Table. Click on the column header to sort the rows 
Table <- gvisTable(Exports, options=list(width=400, height=300))
cat(createGoogleGadget(Table), file="table.xml")

## Table with embedded links
PopTable <- gvisTable(Population, options=list(width=600, height=300, page='enable'))

cat(createGoogleGadget(PopTable), file="poptable.xml")

## Tree Map. Left mouse-click to drill down, right mouse-click to move up a hierarchy
Tree <- gvisTreeMap(Regions,  "Region", "Parent", "Val", "Fac", options=list(fontSize=16))

cat(createGoogleGadget(Tree), file="treemap.xml")

## Annotated Time Line Chart
AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                           numvar="Value", idvar="Device",
                           titlevar="Title", annotationvar="Annotation",
                           options=list(displayAnnotations=TRUE,
                            width=600, height=350)
                           )
cat(createGoogleGadget(AnnoTimeLine), file="annotimeline.xml")

