## File R/GeoMap.R
## Part of the R package gvis
## Copyright 2010 Markus Gesmann, Diego de Castillo
## Distributed under GPL 2 or later

GeoMapPage <- function(data,
                            options=list(width = 600, height=500),
			    htmlHeader=.htmlHeader(paste("GeoMap:", deparse(substitute(data)))),                            
		            htmlFooter=.htmlFooter(),
                            caption=paste("",Sys.time(), R.Version()$version.string, sep="<BR>"),
                            file="",
                            dirname=system.file(file.path("rsp", "myAnalysis"),
                                         package = "gvis"),
                            repos=paste("http://127.0.0.1:8074/",
                                     basename(dirname(system.file(package="gvis"))),
                                     "/gvis/rsp/myAnalysis/", sep=""),
                            view=TRUE
                            ){

    .file <- file
    if(file!=""){
        file <- file.path(dirname, file)
    }

    htmlGeoMap <- GeoMap(data=data,options=options)

    cat(htmlHeader,htmlGeoMap,caption,htmlFooter,file=file,append=TRUE)

    if(.file != "" && view==TRUE){
        .viewGoogleVisualisation(.file, repos=repos)
    }
    return(file)
}


GeoMap <- function(data,options){
    result = gvis(type="GeoMap",data,options)
    result
}

