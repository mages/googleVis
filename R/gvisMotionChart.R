## File R/MotionChart.R
## Part of the R package googleVis
## Copyright 2010 Markus Gesmann, Diego de Castillo
## Distributed under GPL 2 or later

gvisMotionChartPage <- function(data,
                            options=list(gvis=list(width = 600, height=500),
					 data=list(idvar="id",timevar="time",date.format="%Y/%m/%d",
						   allowed="numeric,character,date")),
			    htmlHeader=.htmlHeader(paste("Motion Chart:", deparse(substitute(data)))),                            
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

    htmlMotionChart <- gvisMotionChart(data,options=options)

    cat(htmlHeader,htmlMotionChart,caption,htmlFooter,file=file,append=TRUE)

    if(.file != "" && view==TRUE){
        .viewGoogleVisualisation(.file, repos=repos)
    }
    return(file)
}

gvisMotionChart <- function(data,options){
	result = gvis(type="MotionChart",data=data,options=options)
        result
}


