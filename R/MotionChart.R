## File R/MotionChart.R
## Part of the R package googleVis
## Copyright 2010 Markus Gesmann, Diego de Castillo
## Distributed under GPL 2 or later

MotionChartPage <- function(data,
                            idvar="id",
                            timevar="time",
                            date.format="%Y/%m/%d",
                            options=list(width = 600, height=500),
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

    htmlMotionChart <- MotionChart(data,options=options,idvar=idvar,timevar=timevar,date.format=date.format)

    cat(htmlHeader,htmlMotionChart,caption,htmlFooter,file=file,append=TRUE)

    if(.file != "" && view==TRUE){
        .viewGoogleVisualisation(.file, repos=repos)
    }
    return(file)
}

MotionChart <- function(data,options=list(width = 600,  height=500),
                        idvar=idvar,
                        timevar=timevar,
                        date.format=date.format){

	result = gvis(type="MotionChart",data=data,options=options,idvar=idvar,timevar=timevar,date.format=date.format)
        result
}


