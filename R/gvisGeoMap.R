## File R/GeoMap.R
## Part of the R package googleVis
## Copyright 2010 Markus Gesmann, Diego de Castillo
## Distributed under GPL 2 or later

gvisGeoMapPage <- function(data,
                            options=list(gvis=list(width = 600, height=500),
					 data=list(allowed="numeric,character")),
			    htmlHeader=.htmlHeader(paste("GeoMap:", deparse(substitute(data)))),                            
		            htmlFooter=.htmlFooter(),
                            caption=paste("",Sys.time(), R.Version()$version.string, sep="<BR>"),
                            file="",
                            dirname=system.file(file.path("rsp", "myAnalysis"),
                                         package = "googleVis"),
                            repos=paste("http://127.0.0.1:8074/",
                                     basename(dirname(system.file(package="googleVis"))),
                                     "/googleVis/rsp/myAnalysis/", sep=""),
                            view=TRUE
                            ){

    .file <- file
    if(file!=""){
        file <- file.path(dirname, file)
    }

    htmlGeoMap <- gvisGeoMap(data=data,options=options)

    cat(htmlHeader,htmlGeoMap,caption,htmlFooter,file=file,append=TRUE)

    if(.file != "" && view==TRUE){
        .viewGoogleVisualisation(.file, repos=repos)
    }
    return(file)
}


gvisGeoMap <- function(data,options){
    result = gvis(type="GeoMap",data,options)
    result
}

