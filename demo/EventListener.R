## The parameter gvis.listener.jscode embeds code inside
## a select event listener.
## It is possible to add javascript code and access the build in
## method getSelection().
##
## The following code shows two examples:

jscode <- "window.open('https://en.wikipedia.org/wiki/'+data.getValue(chart.getSelection()[0].row,0));  "
J1 <- gvisGeoMap(Exports, locationvar='Country', numvar='Profit',
                 options=list(dataMode="regions",gvis.listener.jscode=jscode))

plot(J1)
## Similarly the code can also be used for other charts, e.g. org chart and line chart

plot(gvisOrgChart(Regions,  options=list(gvis.listener.jscode=jscode)))

plot(gvisLineChart(Regions[,c(1,3)],  options=list(gvis.listener.jscode=jscode)))


jscode <- "
       var sel = chart.getSelection();
       var row = sel[0].row;
       var text = data.getValue(row,1);
       alert(text);
"

J2 <- gvisTable(Population,options=list(gvis.listener.jscode=jscode))

plot(J2)

## For more details see
## https://developers.google.com/chart/interactive/docs/reference?csw=1#addlistener

## See demo(package='googleVis') for other available demos.
