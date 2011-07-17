### File R/gvis.R
### Part of the R package googleVis
### Copyright 2010, 2011 Markus Gesmann, Diego de Castillo
### Distributed under GPL 2 or later

### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA


gvisChart <- function(type, checked.data, options, chartid, package){
  
  Chart = gvis(type=type, checked.data, options=options, chartid=chartid, package)
  chartid <- Chart$chartid
  htmlChart <- Chart$chart
  
  htmlScaffold <- gvisHtmlWrapper(title="", chartid=chartid, dataName=options$dataName,
                                  type=tolower(type))
  
  output <- structure(
                      list(type=Chart$type,
                           chartid=Chart$chartid,
                           html=list(header=htmlScaffold[["htmlHeader"]],
                             chart=unlist(htmlChart),
                             caption=htmlScaffold[["htmlCaption"]],
                             footer=htmlScaffold[["htmlFooter"]])),
                      class=c("gvis", "list")
                      )
  
  return(output)
}

gvis <- function(type="", data, options, chartid, package){

  if( ! is.data.frame(data) ){
    stop("Data has to be a data.frame. See ?data.frame for more details.")
  }

  ## we need a unique chart id to have more than one chart on the same page
  ## we use type and date to create the chart id
  if(missing(chartid)){
    ##    chartid <- paste(type, format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), basename(tempfile(pattern="")),sep="_")
    chartid <- paste(type, basename(tempfile(pattern="")),sep="ID")
 
  }
  if(missing(package)){
    package <- type
  }

  
  output <- gvisFormat(data)
  data.type <- output$data.type
  data.json <- output$json

  ## check for not allowed data types
  checkTypes <- data.type %in% options$data$allowed 
  if(sum(!checkTypes)){
    message <- paste("Only the following data types are allowed: ", 
	paste(options$data$allowed, collapse=", "), "\n",
	 "However, ", 
	paste(names(data)[!checkTypes], collapse=", "), " is of type ", 
	paste(data.type[!checkTypes], collapse=", "),"\n", sep="", collapse=" ")
    stop(message)
  }  

  jsHeader <- '
<!-- jsHeader -->
<script type="text/javascript\" src="http://www.google.com/jsapi">
</script>
<script type="text/javascript">
'
  jsHeader  <- paste(infoString(type),   jsHeader , sep="\n")

 jsData <- '
// jsData 
function gvisData%s ()
{
  var data = new google.visualization.DataTable();
  var datajson =
%s;
%s
data.addRows(datajson);
return(data);
}
'
  jsData <- sprintf(jsData, chartid,
                     data.json,
                     paste(paste("data.addColumn('", data.type, "','",
                                 names(data.type), "');", sep=""), collapse="\n"))
  
  jsDisplayChart <- '
// jsDisplayChart 
function displayChart%s()
{
  google.load("visualization", "1", { packages:["%s"] %s}); 
  google.setOnLoadCallback(drawChart%s);
}
'
  jsDisplayChart <- sprintf(jsDisplayChart, chartid,
                     tolower(package),
                     ifelse(!is.null(options$gvis$gvis.language),
                            paste(",'language':'",
                                  options$gvis$gvis.language, "'", sep=""), ''),
                     chartid
                     )


  jsDrawChart <- '
// jsDrawChart
function drawChart%s() {
  var data = gvisData%s()
  var chart = new google.visualization.%s(
   document.getElementById(\'%s\')
  );
  var options ={};
%s
  chart.draw(data,options);
  %s
}
'
  jsDrawChart <- sprintf(jsDrawChart, chartid,  chartid, type, chartid,
                     paste(gvisOptions(options), collapse="\n"),
                     gvisListener(chartid, type, options)
                     )



  jsChart <- '
// jsChart 
displayChart%s()
'
   jsChart <- sprintf(jsChart, chartid )

  jsFooter <- '
<!-- jsFooter -->  
//-->
</script>
'

divChart <- '
<!-- divChart -->
<div id="%s"
  style="width: %spx; height: %spx;">
</div>
'
  divChart <- sprintf(divChart, 
		     chartid,
                     ifelse(!(is.null(options$gvis$width) || (options$gvis$width == "")),options$gvis$width,600),
                     ifelse(!(is.null(options$gvis$height) || (options$gvis$height == "")),options$gvis$height,500)
                     )

   output <- list(chart=list(jsHeader=jsHeader,
                                   jsData=jsData,
                                   jsDrawChart=jsDrawChart,
                                   jsDisplayChart=jsDisplayChart,
                                   jsChart=jsChart,
                                   jsFooter=jsFooter,
                                   divChart=divChart),
                     type=type, chartid=chartid)
  return(output)
}

gvisFormat <- function(data){
  
  ## Create a list where the Google DataTable type of all variables will be stored
  require(RJSONIO)
  
  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  varTypes <- sapply(varNames,
                     function(.x){
                       switch(class(x[[.x]])[1],"integer"="number",
                              "numeric"="number",
                              "character"="string",
                              "factor"="string",
                              "logical"="boolean",
                              "Date"="date",
                              "POSIXct"="datetime",
                              "POSIXlt"="datetime")
                     }
                     )
  
  ## factor to character
  x.df <- as.data.frame(
                        lapply(x,
                               function(a){
                                 if (is.factor(a)) as.character(a) else a
                               }
                               ),
                        stringsAsFactors=FALSE
                        )
  ## needed for toJSON, otherwise names are in json-array
  names(x.df) <-  NULL
  x.array <- lapply(seq(nrow(x.df)),
                    function(.row){
                      do.call("list", x.df[.row,])
                    }
                    )

  ## filter out  NA 
  x.array <- rapply(x.array, function(z) if(!is.na(z)){z}, how="list")
  
  output <- list(
                 data.type = unlist(varTypes),
                 json = toJSON(x.array)
                 )

  output$json <-fixBackslash(output$json)

  ## if we have have only one row of data we have to add additional "[" around the json output
  if(nrow(data)==1){
    output$json <- paste("[", output$json ,"]", sep="\n")
  }
  return(output)
}


fixBackslash <- function(x){
   if(packageDescription("RJSONIO")$Version>= "0.7" ){
      x <-  gsub("\\\\\\\\", "\\\\", x)
      }
return(x)
}


check.location <- function(x){
    y = as.character(x)
    if (! is.character(y))
       stop(paste("The column has to be of character format. Currently it is", class(x)))
    y
}

check.char <- function(x){
  y = as.character(x)
  if (! is.character(y))
    stop(paste("The column has to be of character format. Currently it is", class(x)))
  y
}

check.date <- function(x){
    y = as.Date(x)
    if (class(y)!="Date")
       stop(paste("The column has to be of date format. Currently it is", class(x)))
    y
}

check.datetime <- function(x){
  if(! any(class(x) %in% c("POSIXct", "POSIXlt")) )
    stop(paste("The column has to be of datetime format (POSIXct or POSIXlt). Currently it is", class(x)))
  x
}

check.num <- function(x){
    y = as.numeric(x)
    if (! is.numeric(y))
       stop(paste("The column has to be of numeric format. Currently it is", class(x)))
    y
}

check.num.pos <- function(x){
    y = as.numeric(x)
    if (! is.numeric(y))
       stop(paste("The column has to be of numeric format. Currently it is", class(x)))
    if (any(x<0))
       stop(paste("The column has to be > 0."))       
    y
}

gvisCheckData <- function(data="", options=list(), data.structure=list()){
  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  ## Check if required vars match columns in the data
  vars.req <- names(grep(TRUE, lapply(data.structure,
                                      function(x){x$mode})=="required", value=TRUE)) 
  vars.opt <- names(grep(TRUE, lapply(data.structure,
                                      function(x){x$mode})=="optional", value=TRUE)) 
  
  # required and empty? fill with varNames in order given by data.structure
  req.and.empty <- ((names(options$data) %in% vars.req) &
                    (options$data==""))
  
  options$data[req.and.empty] <-
    varNames[match(names(options$data[req.and.empty]),
                   names(data.structure))]  
  
  vars.pos <- match(unlist(options$data[vars.req], use.names=FALSE),
                    varNames) 
  if(any(is.na(vars.pos)) & (length(varNames) < length(vars.req))){
    stop("There are not enough columns in your data.")
  }
  x <- x[as.character(options$data[options$data!="" &
                                   names(options$data) != "allowed" &
                                   names(options$data) != "date.format"])]
  
  sapply(names(options$data[options$data!="" & names(options$data) !=
                            "allowed" & names(options$data) != "date.format"]), 
         function(.x){ 
           .x <- as.character(.x)
           y <- x[[as.character(options$data[.x])]];
           x[[as.character(options$data[.x])]] <<- data.structure[[.x]]$FUN(y);
         })
  
  return(x)
}

gvisListener <- function(chartid, type, options=list(gvis=list(gvis.listener.jscode = NULL))){
    event <- "select"
    jscode <- options$gvis$gvis.listener.jscode
    jsListener <- ""
    # not all types support Listener and select event
    if(!is.null(jscode)){## this actually works in most & (tolower(type) %in% c("table", "geomap"))){
        jsListener <- "
  google.visualization.events.addListener(chart, '%s',gvisListener%s);
  function gvisListener%s() {
      %s
  }
"
        jsListener <- sprintf(jsListener,event,chartid,chartid,jscode)
    }
    jsListener
}


gvisOptions <- function(options=list(gvis=list(width = 600, height=500))){
    options <- options$gvis
    # wipe out options with start with gvis.
    options[grep("^gvis.",names(options))] <- NULL
    .par <- sapply(names(options), function(x)
                   paste("options[\"", x,"\"] = ",
                                ifelse(any(checkSquareCurlBracketOps(options[[x]])),
                                       paste(options[[x]], collapse="\n"),
                                       paste(toJSON(options[[x]], FALSE), collapse="\n")                                       
                                       ),
                         ";",sep="" )
                   )
    return(fixBackslash(.par))
}


checkSquareCurlBracketOps <- function(char){

  first <- substr(char, 1,1)
  n <- nchar(char)
  last <- substr(char, n, n)
  ifelse(first %in% c("{","[") & last %in% c("}", "]"), TRUE, FALSE)
}

gvisHtmlWrapper <- function(title, dataName, chartid, type){

  htmlHeader <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <title>%s</title>
  <meta http-equiv="content-type" content="text/html;charset=utf-8" />
  <style type="text/css">
    body {
          color: #444444;
          font-family: Arial,Helvetica,sans-serif;
          font-size: 75%%;
    }
    a {
          color: #4D87C7;
          text-decoration: none;
    }
  </style>
</head>
<body>
'
  htmlHeader <- sprintf(htmlHeader,chartid) 

 htmlFooter <- '
<!-- htmlFooter -->
<span> 
%s &#8226; <a href="http://code.google.com/p/google-motion-charts-with-r/">googleVis-%s</a>
&#8226; <a href="http://code.google.com/apis/visualization/terms.html">Google Terms of Use</a> &#8226; %s
</span></div>
</body>
</html>
'

if(type %in% "gvisMerge"){
  policy <- "Data Policy: See individual charts"
}else{
  policy <- sprintf('<a href="http://code.google.com/apis/chart/interactive/docs/gallery/%s.html#Data_Policy">Data Policy</a>', type)
}

  htmlFooter <- sprintf(htmlFooter, R.Version()$version.string,
		        packageDescription('googleVis')$Version, policy)  
  htmlCaption <- sprintf('<div><span>Data: %s &#8226; Chart ID: <a href="Chart_%s.html">%s</a></span><br />' ,
                         dataName, chartid, chartid)

  return(list(htmlHeader=htmlHeader,
              htmlFooter=htmlFooter,
              htmlCaption=htmlCaption
              ))
}


## taken from lattice by Deepayan Sarkar
modifyList <- function (x, val) {
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    for (v in names(val)) {
        x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
            modifyList(x[[v]], val[[v]])
        else val[[v]]
    }
    x
}

## info string 

infoString <- function(type=""){
    result <- string("",file="",append=FALSE)
    info <- R.Version()
    BCOMMENT <- "<!-- "
    ECOMMENT <- " -->\n"

    result <- result + BCOMMENT + type + " generated in " +
      info$language + " " + info$major + "." + info$minor + 
        " by googleVis " +
          packageDescription('googleVis')$Version + " package" +
            ECOMMENT 
    result <- result + BCOMMENT + date() + ECOMMENT
    result$text
  }

## define class string with nice '+' paste, taken from xtable.

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


