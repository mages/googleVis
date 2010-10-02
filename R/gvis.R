## File R/googleVis.R
## Part of the R package googleVis
## Copyright 2010 Markus Gesmann, Diego de Castillo
## Distributed under GPL 2 or later

gvis <- function(type="",data,options){

    output <- gvisFormat(type=type,data,options)
    data.type <- output$data.type
    data.json <- output$json
    
    jsTableTemplate <- '
     <script type="text/javascript\" src="http://www.google.com/jsapi"></script>
     <script type="text/javascript">
      google.load("visualization", "1", { packages:["%s"] });
      google.setOnLoadCallback(drawChart);
      function drawChart() {
        var data = new google.visualization.DataTable();
        var datajson = %s;
	%s
	data.addRows(datajson);
        var chart = new google.visualization.%s(document.getElementById(\'chart_div\'));
        var options ={};
        %s
        chart.draw(data,options);
      }
     </script>
     <div id="chart_div"></div>
    '
    jsTable <- sprintf(jsTableTemplate,
			tolower(type),
			data.json,
	                paste(paste("data.addColumn('",data.type,"','",
				    names(data.type),"');",sep=""),collapse="\n"),
			type,
		        paste(.setOptions(options),collapse="\n"))

    jsTable <- paste(infoString(type),jsTable,sep="\n")
    return(jsTable)
}

gvisFormat <- function(type,data,options){

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

    if (type == "MotionChart"){
    ## Check if idvar and timevar exist
    idvar.timevar.pos <- match(c(options$data$idvar, options$data$timevar), varNames)
    if(sum(!is.na(idvar.timevar.pos)) < 2){
        stop("There is a missmatch between the idvar and timevar specified and the colnames of your data.")
    }

    ## Check if timevar is either a numeric or date
    if( is.numeric(x[[options$data$timevar]]) | class(x[[options$data$timevar]])=="Date" ){
        typeMotionChart[[options$data$timevar]] <- ifelse(is.numeric(x[[options$data$timevar]]), "number",
                                             ##ifelse(date.format %in% c("%YW%W","%YW%U"), "string",
                                             "date")##)
    }else{
        stop(paste("The timevar has to be of numeric or Date format. Currently it is", class(x[[options$data$timevar]])))
    }

    ## idvar has to be a character, so lets try to convert it into a character
    if( ! is.character(x[[options$data$idvar]]) ){
        x[[options$data$idvar]] <- as.character(x[[options$data$idvar]])
    }
    typeMotionChart[[options$data$idvar]] <- "string"

    varOthers <- varNames[ -idvar.timevar.pos  ]
    varOrder <- c(options$data$idvar, options$data$timevar, varOthers)
    x <- x[varOrder]

    typeMotionChart[varOthers] <- sapply(varOthers, function(.x) ifelse(is.numeric(x[[.x]]), "number","string"))
    typeMotionChart <- typeMotionChart[varOrder]
    x[varOthers] <- lapply(varOthers,function(.x){ if(class(x[[.x]])=="Date") as.character(x[[.x]]) else x[[.x]]})
    } else {
    typeMotionChart <- sapply(varNames, function(.x) ifelse(is.numeric(x[[.x]]), "number",ifelse(is.logical(x[[.x]]),"boolean","string")))
    x <- lapply(varNames,function(.x){ if(class(x[[.x]])=="Date") as.character(x[[.x]]) else x[[.x]]})
    }
    # factor to character, date to character
    x.df <- as.data.frame(lapply(x,function(a) if (is.factor(a)) as.character(a) else a),stringsAsFactors=F)
    # needed for toJSON, otherwise names are in json-array
    names(x.df) <-  NULL
    x.array <- lapply(seq(nrow(x.df)),function(.row){do.call("list",x.df[.row,])})

    output <- list(
                   data.type = unlist(typeMotionChart),
                   json = toJSON(x.array)
                   )

    return(output)
}


.setOptions <- function(options=list(gvis=list(width = 600, height=500))){
    options <- options$gvis
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




.viewGoogleVisualisation <- function(file, repos=paste("http://127.0.0.1:8074/",
                                          basename(dirname(system.file(package="googleVis"))),
                                          "/googleVis/rsp/myAnalysis/", sep="")){

    browseRsp(paste(repos, file, sep=""))
}

.htmlHeader <- function(title){
    htmlTemplateHeader <- '
     <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
       "http://www.w3.org/TR/REC-html40/loose.dtd"> <%%title="%s"%%>
     <html>
     <%%@include file="../src/simpleHead.rsp"%%>
     <body>
     <%%@include file="../src/simpleHeader.rsp"%%>
    '
    
    htmlHeader <- sprintf(htmlTemplateHeader,title)
    return(htmlHeader)
}

.htmlFooter <- function(){
    htmlFooter <- '
     <%@include file="../src/simpleFooter.rsp"%>
     </body>
     </html>
    '
    return(htmlFooter)
}

# info string 

infoString <- function(type=""){
    result <- string("",file="",append=FALSE)
    info <- R.Version()
    BCOMMENT <- "<!-- "
    ECOMMENT <- " -->\n"

    result <- result + BCOMMENT + type + " generated in " +
              info$language + " " + info$major + "." + info$minor + 
              " by googleVis " + packageDescription('googleVis')$Version + " package" + ECOMMENT
    result <- result + BCOMMENT + date() + ECOMMENT
    result$text
}

# define class string with nice '+' paste, taken from xtable.

"+.string" <- function(x,y) {
  x$text <- paste(x$text,as.string(y)$text,sep="")
  return(x)
}

print.string <- function(x,...) {
  cat(x$text,file=x$file,append=x$append)
  return(invisible())
}

string <- function(text,file="",append=FALSE) {
  x <- list(text=text,file=file,append=append)
  class(x) <- "string"
  return(x)
}

as.string <- function(x,file="",append=FALSE) {
  if (is.null(attr(x,"class",exact=TRUE)))
  switch(data.class(x),
      character=return(string(x,file,append)),
      numeric=return(string(as.character(x),file,append)),
      stop("Cannot coerse argument to a string"))
  if (class(x)=="string")
    return(x)
  stop("Cannot coerse argument to a string")
}

is.string <- function(x) {
  return(class(x)=="string")
}


