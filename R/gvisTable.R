## File R/Table.R
## Part of the R package googleVis
## Copyright 2010 Markus Gesmann, Diego de Castillo
## Distributed under GPL 2 or later

gvisTablePage <- function(data,
                            options=list(width = 600, height=500),
			    htmlHeader=.htmlHeader(paste("Table:", deparse(substitute(data)))),                            
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

    htmlTable <- gvisTable(data=data,options=options)

    cat(htmlHeader,htmlTable,caption,htmlFooter,file=file,append=TRUE)

    if(.file != "" && view==TRUE){
        .viewGoogleVisualisation(.file, repos=repos)
    }
    return(file)
}


gvisTable <- function(data,options){
    result = gvis(type="Table",data,options)
    result
}

