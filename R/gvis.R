### File R/gvis.R
### Part of the R package googleVis
### Copyright 2010 - 2014 Markus Gesmann, Diego de Castillo
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


gvisChart <- function(type, checked.data, options, chartid, package, formats = NULL){
  
  Chart = gvis(type=type, checked.data, options=options, 
               chartid=chartid, package, formats=formats)
  chartid <- Chart$chartid
  htmlChart <- Chart$chart
  
  htmlScaffold <- gvisHtmlWrapper(title="", chartid=chartid, 
                                  dataName=options$dataName,
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

gvis <- function(type="", data, options, chartid, package, formats=NULL){
  
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
  checkTypes <- data.type %in% options$data$allowed | isRoleColumn(data.type)
  if(sum(!checkTypes)){
    message <- paste("Only the following data types are allowed: ", 
                     paste(options$data$allowed, collapse=", "), "\n",
                     "However, ", 
                     paste(names(data)[!checkTypes], collapse=", "), " is of type ", 
                     paste(data.type[!checkTypes], collapse=", "),"\n", sep="", collapse=" ")
    stop(message)
  }  
  
  ## check for NumberFormat objects
  ## formats will be a named list of strings. Names correspond to objects in the data frame.
  jsFormats <- ''
  if (!is.null(formats)) {
    if (!inherits(formats,'list'))
      warning('formats object exists but is not a list. Ignoring')
    
    if (!all(names(formats) %in% names(data))) 
      warning('formats object contains names that do not exist in data.')
    
    for (idx in c(1:length(formats))) {
      jsFormats <- paste(jsFormats
                         , paste('  var dataFormat',idx,' = new google.visualization.NumberFormat({pattern:"',formats[idx],'"});',sep="")
                         , paste('  dataFormat',idx,'.format(data, ', match(names(formats[idx]), names(data)) - 1,');',sep="")
                         , sep = "\n"
      )
    }
    
  }
  
  jsHeader <- '
<!-- jsHeader -->
<script type="text/javascript">
'
  jsHeader  <- paste(infoString(type),   jsHeader , sep="\n")
  
  jsData <- '
// jsData 
function gvisData%s () {
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
                    paste(sapply(1:length(data.type), function(x)
                      if (endsIn(names(data.type)[x], '.tooltip'))
                        paste("data.addColumn({type: '", data.type[x], "', role: 'tooltip', 'p': {'html': true}});", sep="")
                      else if(endsIn.No(names(data.type)[x], '.interval')){
                        id <- unlist(strsplit(names(data.type)[x], ".interval."))
                        id <- id[length(id)]
                        paste("data.addColumn({id:'i",id,"', type: '", data.type[x], "', role: 'interval'});", sep="")
                      }
                      else if(endsIn(names(data.type)[x], '.style'))
                        paste("data.addColumn({type: '", data.type[x], "', role: 'style'});", sep="")                                 
                      else if(endsIn(names(data.type)[x], '.annotation'))
                        paste("data.addColumn({type: '", data.type[x], "', role: 'annotation'});", sep="")
                      else if(endsIn(names(data.type)[x], '.annotationText'))
                        paste("data.addColumn({type: '", data.type[x], "', role: 'annotationText'});", sep="")
                      else if(endsIn(names(data.type)[x], '.certainty'))
                        paste("data.addColumn({type: '", data.type[x], "', role: 'certainty'});", sep="")
                      else if(endsIn(names(data.type)[x], '.scope'))
                        paste("data.addColumn({type: '", data.type[x], "', role: 'scope'});", sep="")
                      else if(endsIn(names(data.type)[x], '.emphasis'))
                        paste("data.addColumn({type: '", data.type[x], "', role: 'emphasis'});", sep="")
                      else
                        paste("data.addColumn('", data.type[x], "','",
                              names(data.type)[x], "');", sep="")
                    ), collapse="\n")
                    
  )
  
  jsDisplayChart <- '
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "%s";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChart%s);
})();
function displayChart%s() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} %s});
}, 100);
}
'
  jsDisplayChart <- sprintf(jsDisplayChart,
                            ifelse(!is.null(options$gvis$gvis.editor), 'charteditor',
                                   tolower(package)),
                            chartid, chartid,
                            ifelse(!is.null(options$gvis$gvis.language),
                                   paste(",'language':'",
                                         options$gvis$gvis.language, "'", sep=""), '')
  )
  
  #########
  
  jsDrawChart <- '
// jsDrawChart
function drawChart%s() {
var data = gvisData%s();
var options = {};
%s
%s
%s
%s
}
%s  
'
  jsDrawChart <- sprintf(jsDrawChart, chartid,  chartid,
                         paste(gvisOptions(options), collapse="\n"),
                         jsFormats,
                         gvisNewChart(chartid,type,options),
                         gvisListener(chartid, type, options),
                         gvisEditor(chartid,type,options)
  )
  
  jsFooter  <- '
// jsFooter
</script>
'
  
  jsChart <- '
<!-- jsChart -->  
<script type="text/javascript\" src="https://www.google.com/jsapi?callback=displayChart%s"></script>
'
  jsChart  <- sprintf(jsChart, chartid)
  
  
  divChart <- '
<!-- divChart -->
%s  
<div id="%s" 
  style="width: %s; height: %s;">
</div>
'  
  divChart <- sprintf(divChart,
                      ifelse(!is.null(options$gvis$gvis.editor),
                             sprintf("<input type='button' onclick='openEditor%s()' value='%s'/>",
                                     chartid,as.character(options$gvis$gvis.editor)),''),
                      chartid,
                      ifelse(!(is.null(options$gvis$width) || (options$gvis$width == "")),options$gvis$width, "500"),
                      ifelse(!(is.null(options$gvis$height) || (options$gvis$height == "")),options$gvis$height, "automatic")
  )
  
  output <- list(chart=list(jsHeader=jsHeader,
                            jsData=jsData,
                            jsDrawChart=jsDrawChart,
                            jsDisplayChart=jsDisplayChart,
                            jsFooter=jsFooter,
                            jsChart=jsChart,
                            divChart=divChart),
                 type=type, chartid=chartid)
  return(output)
}



###############
## Thanks to Sebastian Kranz for this function
## Thanks also to Wei Luo: https://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis

# Transforms a vector into a vector of JSON strings that
# can be pasted together afterwards
toJsonSONvec <- function(vec) {
  #restore.point("name.value")
  na.row <- is.na(vec)
  if(is(vec,'integer')){
    ret <- vec
  } else if (is(vec,'numeric')) {
    # Round to 10 points after the decimal as before
    ret <- as.character(signif(vec,digits=10))
  } else if (is(vec,'logical')) {
    ret <- tolower(as.character(vec))
  } else if (is(vec,'Date')) {
    y <- format(vec,"%Y")
    m <- as.numeric(format(vec,"%m")) -1
    d <- as.numeric(format(vec,"%d"))
    ret <- paste("new Date(",y,",",m,",",d,")",sep="")
  } else if (is(vec,'POSIXct') | is(vec,'POSIXlt')) {
    y <- format(vec,"%Y")
    m <- as.numeric(format(vec,"%m")) -1
    d <- as.numeric(format(vec,"%d"))
    H <- as.numeric(format(vec,"%H"))
    M <- as.numeric(format(vec,"%M"))
    S <- as.numeric(format(vec,"%S"))
    ret <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
  } else {
    quote <- '"';
    vec <- gsub('"', '\\\\"', vec)
    ret <- paste(quote, vec, quote, sep='')
  }
  ret[na.row] <- "null"
  ret
}

toJsonSONarray <- function(dtf){
  ## Thanks to Sebastian Kranz for this function
  ## Thanks also to Wei Luo: https://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis
  
  #restore.point("toJsonSONarray")
  clnms <- colnames(dtf)
  
  # Transform columns depending on data type and store in a list
  objs <- lapply(dtf,toJsonSONvec)
  # Remove names just for the case that one column name was sep or collapse
  names(objs) <- NULL
  # Paste columns together
  str <- do.call(paste,c(objs,list(sep=",\n")))
  
  # Add [ ] and paste rows together
  res <- paste('[\n ', paste0("[\n",str,"\n]",collapse=',\n'), ' \n]',sep="")
  return(res)
}


gvisFormat <- function(data){
  #restore.point("gvisFormat")
  ## Create a list where the Google DataTable type of all variables will be stored
  
  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  varTypes <- sapply(varNames,
                     function(.x){
                       switch(class(x[[.x]])[1],"integer"="number",
                              "numeric"="number",
                              "character"="string",
                              "factor"="string",
                              "ordered"="string",
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
  
  ## The function is specified above
  json <- toJsonSONarray(x.df)
  output <- list(
    data.type = unlist(varTypes),
    json = json
  )
  
  output$json <-fixBackslash(output$json)
  
  return(output)
}

fixBackslash <- function(x){
  x <-  gsub("\\\\\\\\", "\\\\", x)
  return(x)
}

endsIn <- function(source, target){
  substr(source, nchar(source)-nchar(target)+1, nchar(source)) %in% target
}

endsIn.No <- function(source, target){
  ind <- grep(paste0(target, ".[0-9]"), source) 
  c(1:length(source)) %in% ind    
}

isRoleColumn <- function(x) {
  return (
    (x %in% c("string") & endsIn(names(x), ".tooltip")) |
      (x %in% c("string") & endsIn(names(x), ".annotation")) |
      (x %in% c("string") & endsIn(names(x), ".style")) |
      (x %in% c("string") & endsIn(names(x), ".annotationText")) |
      (x %in% c("boolean") & endsIn(names(x), ".certainty")) |
      (x %in% c("boolean") & endsIn(names(x), ".emphasis")) |
      (x %in% c("boolean") & endsIn(names(x), ".scope")) |
      (x %in% c("number") & endsIn.No(names(x), ".interval"))
  )
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

check.char.num <- function(x){
  if(!(( is.numeric(x) | is.character(x)) | is.factor(x)))
    stop(paste("The column has to be of numeric or character format. Currently it is", class(x)))
  x
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
           # is <<- really necessary?
           x[[as.character(options$data[.x])]] <<- data.structure[[.x]]$FUN(y);
         })
  
  return(x)
}

gvisEditor <- function(chartid,type,options){
  if(is.null(options$gvis$gvis.editor))
    return('')
  jseditor <- "
  function openEditor%s() {
  var editor = new google.visualization.ChartEditor();
  google.visualization.events.addListener(editor, 'ok',
  function() { 
  chart%s = editor.getChartWrapper();  
  chart%s.draw(document.getElementById('%s')); 
  }); 
  editor.openDialog(chart%s);
  }
  "
  sprintf(jseditor,chartid,chartid,chartid,chartid,chartid)
  }

gvisNewChart <- function(chartid,type,options){
  ret <- ""
  if(is.null(options$gvis$gvis.editor)){
    jsnewchart <- "
    var chart = new google.visualization.%s(
    document.getElementById(\'%s\')
    );
    chart.draw(data,options);
    "
    ret <- sprintf(jsnewchart,type,chartid)  
    
  } else {
    jsnewchart <- "
    chart%s = new google.visualization.ChartWrapper({
    dataTable: data,       
    chartType: '%s',
    containerId: '%s',
    options: options
    });
    chart%s.draw();
    "
    ret <- sprintf(jsnewchart,chartid,type,chartid,chartid)
  }
  ret
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
  #print(options)
  .par <- sapply(names(options), function(x){
    if(! (x %in% c("width", "height") & 
            ! is.numeric(type.convert(gsub("px","", options[[x]])))) 
  ){
   
      paste("options[\"", x,"\"] = ",
            ifelse(any(checkSquareCurlBracketOps(options[[x]])),
                   paste(options[[x]], collapse="\n"),
                   #paste0( toJsonSONvec(options[[x]]), collapse="\n") 
                   paste0( toJSON(unbox(options[[x]])), collapse="\n")  
            ),
            ";",sep="" )      
      
      
    }
  }
  )
 .par <- .par[!sapply(.par,is.null)]
 # print(.par)
  return((.par))
}


checkSquareCurlBracketOps <- function(char){
  
  first <- substr(char, 1,1)
  n <- nchar(char)
  last <- substr(char, n, n)
  ifelse(first %in% c("{","[") & last %in% c("}", "]"), TRUE, FALSE)
}

gvisHtmlWrapper <- function(title, dataName, chartid, type){
  
  htmlHeader <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "https://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="https://www.w3.org/1999/xhtml">
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
  %s 
  &#8226; <a href="https://developers.google.com/terms/">Google Terms of Use</a> &#8226; %s
</span></div>
</body>
</html>
'
  
  if(type %in% "gvisMerge"){
    policy <- "Data Policy: See individual charts"
  }else{
    policy <- sprintf('<a href="https://google-developers.appspot.com/chart/interactive/docs/gallery/%s">Documentation and Data Policy</a>', type)
  }
  
  htmlFooter <- sprintf(htmlFooter, R.Version()$version.string,
                         policy)  
  htmlCaption <- sprintf('<div><span>Data: %s &#8226; Chart ID: <a href="Chart_%s.html">%s</a> &#8226; <a href="https://github.com/mages/googleVis">googleVis-%s</a></span><br />' ,
                         dataName, chartid, chartid,packageDescription('googleVis')$Version)
  
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
