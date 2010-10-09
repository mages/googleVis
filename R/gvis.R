### File R/gvis.R
### Part of the R package googleVis
### Copyright 2010 Markus Gesmann, Diego de Castillo
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


gvisChart <- function(type, checked.data, options){
  
  Chart = gvis(type=type, checked.data, options=options)
  
  chartid <- Chart$chartid
  htmlChart <- Chart$jsChart
  
  htmlScaffold <- gvisHtmlWrapper(title="", chartid=chartid, dataName=options$dataName)
  
  output <- list(type=Chart$type,
                 chartid=Chart$chartid,
                 html=list(Header=htmlScaffold[["htmlHeader"]],
                   Chart=htmlChart,
                   Caption=htmlScaffold[["htmlCaption"]],
                   Footer=htmlScaffold[["htmlFooter"]]
                   ))
  
  class(output) <- c("gvis", class(output))
  
  return(output)
}

gvis <- function(type="", data, options, chartid=NULL){

  if( ! is.data.frame(data) ){
    stop("Data has to be a data.frame. See ?data.frame for more details.")
  }

  ## we need a unique chart id to have more than one chart on the same page
  ## we use type and date to create the chart id
  if(is.null(chartid)){
    chartid <- paste(type, format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), sep="_")
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

  jsTableTemplate <- paste('
     <script type="text/javascript\" src="http://www.google.com/jsapi"></script>
     <script type="text/javascript">
      google.load("visualization", "1", { packages:["%s"] });
      google.setOnLoadCallback(drawChart);
      function drawChart() {
        var data = new google.visualization.DataTable();
        var datajson = %s;
	%s
	data.addRows(datajson);
        var chart = new google.visualization.%s(document.getElementById(\'',chartid,'\'));
        var options ={};
        %s
        chart.draw(data,options);
      }
     </script>
     <div id="', chartid,'"></div>
    ', sep='')
  
  jsChart <- sprintf(jsTableTemplate,
                     tolower(type),
                     data.json,
                     paste(paste("data.addColumn('", data.type, "','",
                                 names(data.type), "');", sep=""), collapse="\n"),
                     type,
                     paste(gvisOptions(options), collapse="\n")
                     )
  
  jsChart <- paste(infoString(type), jsChart, sep="\n")
  
  ## return json object and chart id
  
  output <- list(jsChart=jsChart, type=type, chartid=chartid)

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
                       switch(class(x[[.x]]),"integer"="number",
					     "numeric"="number",
					     "character"="string",
					     "factor"="string",
					     "logical"="boolean",
						"Date"="date")})
  
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
  
  output <- list(
                 data.type = unlist(varTypes),
                 json = toJSON(x.array)
                 )
  
  return(output)
}

gvisOptions <- function(options=list(gvis=list(width = 600, height=500))){
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


gvisHtmlWrapper <- function(title, dataName, chartid){

  htmlHeader <- '
     <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
       "http://www.w3.org/TR/REC-html40/loose.dtd"> <%%title="%s"%%>

     <html>
     <%%@include file="../src/simpleHead.rsp"%%>
     <body>
     <%%@include file="../src/simpleHeader.rsp"%%>
    '
  
  htmlHeader <- sprintf(htmlHeader,title)

  htmlFooter <- '
     <%@include file="../src/simpleFooter.rsp"%>
     </body>
     </html>\n'

  googleTerms <- '<a href="http://code.google.com/apis/visualization/terms.html">Google Terms of Use</a>'
    
  htmlCaption <- sprintf('Data: %s, Chart ID: %s<BR>%s, %s<BR><BR>',
                         dataName, chartid, R.Version()$version.string, googleTerms)

  return(list(htmlHeader=htmlHeader,
              htmlFooter=htmlFooter,
              htmlCaption=htmlCaption))
}


## taken from lattice
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


