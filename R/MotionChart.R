## File R/MotionChart.R
## Part of the R package GoogleMotionChart
## Copyright 2010 Markus Gesmann
## Distributed under GPL 2 or later

MotionChartPage <- function(data,
                            idvar="id",
                            timevar="time",
                            date.format="%Y/%m/%d",
                            options=list(width = 600, height=500),
                            title=paste("Motion Chart:", deparse(substitute(data))),
                            caption=paste("",Sys.time(), R.Version()$version.string, sep="<BR>"),
                            file="",
                            dirname=system.file(file.path("rsp", "myAnalysis"),
                                         package = "GoogleMotionChart"),
                            repos=paste("http://127.0.0.1:8074/",
                                     basename(dirname(system.file(package="GoogleMotionChart"))),
                                     "/GoogleMotionChart/rsp/myAnalysis/", sep=""),
                            view=TRUE
                            ){

    .file <- file
    if(file!=""){
        file <- file.path(dirname, file)
    }

    cat(paste(#"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">",
              #"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n",

"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"\n",
       " \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n",


              "<%title=\"", title, "\"%>\n<html>\n",
              "<%@include file=\"../src/simpleHead.rsp\"%>\n",
              "<body>\n",
              "<%@include file=\"../src/simpleHeader.rsp\"%>\n",
               sep=""),
        file=file)

    MotionChart(data, idvar, timevar,date.format=date.format,
                options=options,
                file=file, append=TRUE)

    cat(caption, file=file, append=TRUE)

    cat(paste("\n<%@include file=\"../src/simpleFooter.rsp\"%>\n",
              "\n</body>\n</html>\n"), file = file, append=TRUE)

    if(.file != "" && view==TRUE){
        .viewGoogleVisualisation(.file, repos=repos)
    }
    return(file)
}

MotionChart <- function(data,
                        idvar="id",
                        timevar="time",
                        date.format="%Y/%m/%d",
                        options=list(width = 600,  height=500),
                        file="",
                        append=TRUE){

    output <- formatGoogleChartData(data, idvar, timevar, date.format)
    data <- output$data
    data.type <- output$data.type

    names(data) <- NULL
    as.numlist <- function(x){
      fnn <- function(a){
        b <- suppressWarnings(as.numeric(a))
        if (is.na(b)) a else b
      }
      lapply(as.list(x),fnn)
    }
    data.json <- toJSON(apply(as.matrix(data),1,as.numlist))
    data.json <-  gsub(')\"',')',gsub('\"new Date','new Date',data.json))

    jsMotionChartTemplate <- '
     <script type="text/javascript\" src="http://www.google.com/jsapi"></script>
     <script type="text/javascript">
      google.load("visualization", "1", { packages:["motionchart"] });
      google.setOnLoadCallback(drawChart);
      function drawChart() {
        var data = new google.visualization.DataTable();
        var datajson = %s;
	%s
	data.addRows(datajson);
        var chart = new google.visualization.MotionChart(document.getElementById(\'chart_div\'));
        var options ={};
        %s
        chart.draw(data,options);
      }
     </script>
     <div id="chart_div"></div>
    '

    jsMotionChart <- sprintf(jsMotionChartTemplate,
			data.json,
	                paste(paste("data.addColumn('",data.type,"','",
				    names(data.type),"');",sep=""),collapse="\n"),
		        paste(.setMotionChartOptions(options),collapse="\n"))

    cat(jsMotionChart,sep="\n", file=file, append=append)
    return(file)
}

.viewGoogleVisualisation <- function(file, repos=paste("http://127.0.0.1:8074/",
                                          basename(dirname(system.file(package="GoogleMotionChart"))),
                                          "/GoogleMotionChart/rsp/myAnalysis/", sep="")){

    browseRsp(paste(repos, file, sep=""))
}


formatGoogleChartData <- function(data, idvar="id", timevar="time",  date.format="%Y/%m/%d"){

    ## Create a list where the Google DataTable type of all variables will be stored
    ## Google Motion Chart needs a 'string' in the id variable (first column)
    ## A number or date in the time variable (second column)
    ## Everything else has to be a number or string

    if( ! is.data.frame(data) ){
        stop("Data has to be a data.frame. See ?data.frame for more details.")
    }

    ## Convert data.frame to list
    x <- as.list(data)
    varNames <- names(x)


    ## typeMotionChart will hold the Google DataTable formats of our data
    typeMotionChart <- as.list(rep(NA, length(varNames)))
    names(typeMotionChart) <- varNames

    ## Check if idvar and timevar exist
    idvar.timevar.pos <- match(c(idvar, timevar), varNames)
    if(sum(!is.na(idvar.timevar.pos)) < 2){
        stop("There is a missmatch between the idvar and timevar specified and the colnames of your data.")
    }

    ## Check if timevar is either a numeric or date
    if( is.numeric(x[[timevar]]) | class(x[[timevar]])=="Date" ){
        typeMotionChart[[timevar]] <- ifelse(is.numeric(x[[timevar]]), "number",
                                             ##ifelse(date.format %in% c("%YW%W","%YW%U"), "string",
                                             "date")##)
    }else{
        stop(paste("The timevar has to be of numeric or Date format. Currently it is", class(x[[timevar]])))
    }

    ## idvar has to be a character, so lets try to convert it into a character
    if( ! is.character(x[[idvar]]) ){
        x[[idvar]] <- as.character(x[[idvar]])
    }
    typeMotionChart[[idvar]] <- "string"

    varOthers <- varNames[ -idvar.timevar.pos  ]
    ## check if all other variables are either numeric or strings
    typeMotionChart[varOthers] <- sapply(varOthers, function(.x) ifelse(is.numeric(x[[.x]]), "number","string"))

    varOrder <- c(idvar, timevar, varOthers)

    x <- lapply(varOrder,
                function(n){
                    ifelse(typeMotionChart[[n]] == "number",
                           return(x[[n]]),
                           ifelse(typeMotionChart[[n]] == "date",
                                  return(paste("new Date('", format(x[[n]], date.format), "')", sep="")),
                                  return(x[[n]])
                                  )
                           )
                }
                )
    names(x) <-  varOrder

    output <- list(
                   data.type= unlist(typeMotionChart[varOrder]),
                   data = as.data.frame(x)
                   )

    return(output)
}


.setMotionChartOptions <- function(options=list(width = 600, height=500)){

    .par <- sapply(names(options), function(x)
                   paste("                 options[\"", x,"\"] = ",
                         ifelse(is.numeric(options[[x]]) | is.logical(options[[x]]),
                                ifelse(is.numeric(options[[x]]),
                                       options[[x]],
                                       ifelse(options[[x]],
                                              "true", "false")
                                       ),
                                paste("'", options[[x]],"'",sep="")),
                         ";",sep="" )
                   )

    return(.par)
}
