### File R/gvisMethods.R
### Part of the R package googleVis
### Copyright 2010, 2011, 2012, 2013 Markus Gesmann, Diego de Castillo
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

#' @export
print.gvis <- function(x, tag=NULL, file="", ...){

  if(is.null(tag))
    tag <- getOption("gvis.print.tag")
  
  if(!tag %in% getOption("gvis.tags"))
        stop(paste(tag,
        "is not a valid option. Set tag to NULL or one of the following:\n",
                   paste(getOption("gvis.tags"), collapse=", "))) 
      
  tag <- ifelse( tag %in% c("chartid", "type", "html"), tag, paste(".", tag, sep=""))

  output <- unlist(x)
  tag.names <- names(output)
  .id <- apply(t(tag), 2, function(y)
               grep(paste("\\", y, sep=""), tag.names)
               )
  cat(output[.id], file=file, ...)
  
}


isServerRunning <- function() {
  #tools:::httpdPort > 0L
  ifelse(R.version['svn rev'] < 67550 | getRversion() < "3.2.0",
         get("httpdPort", envir=environment(startDynamicHelp))>0,
         tools::startDynamicHelp(NA)>0
  )  
}

googlevis.httpd.handler <- function(path, query, ...) {
  path <- gsub("^/custom/googleVis/", "", path)
  f <- sprintf("%s%s%s",
               tempdir(),
               .Platform$file.sep,
                 path) 
  list(file=f,
       "content-type"="text/html",
       "status code"=200L)
}

#' @export
plot.gvis <- function(x, tag=NULL, ...){

  if(missing(tag)) ## Has tag being actively set?
    tag <- getOption("gvis.plot.tag")

  if(is.null(tag) | !('gvis' %in% class(x))){  ## Open browser window if tag is NULL   
    if(!isServerRunning() ) {
      startDynamicHelp()
    }
    
    env <- get( ".httpd.handlers.env", asNamespace("tools"))
    env[["googleVis"]] <- googlevis.httpd.handler    
    root.dir <- tempdir()
    
    ## Write the whole visualisation into a html file
    if('gvis' %in% class(x)){          
      ## Write the pure chart html code into a separate file
      chart.txt <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
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
<p>
  You find below the HTML code of the visualisation.<br />
  You can copy and paste the code into an existing HTML page.<br />
  For more information see also <a href="/library/googleVis/html/gvisMethods.html">?print.gvis</a></p>
<p><textarea rows="50" name="html" cols="80">
%s
</textarea>
</p>
</body>
</html>
'    
      chart.txt <- sprintf(chart.txt, x$chartid,gsub(">","&gt;",gsub("<","&lt;",
                                                                     paste(unlist(x$html$chart), collapse="\n"))))
      cat(chart.txt, file=file.path(root.dir, paste("Chart_", x$chartid, ".html", sep="")))      
      file <- file.path(root.dir, paste(x$chartid ,".html", sep=""))
    }else{## not a gvis object
      basex <- basename(x)
      if(length(grep("htm", substr(basex, nchar(basex)-3,nchar(basex)))) < 1)
        warning("The file does not appear to be an html file.\n")
      file.copy(from=x, to=file.path(root.dir, basex),...)
      file <- file.path(root.dir, basex)
    }    
    print(x, file=file)    
    .url <- sprintf("http://127.0.0.1:%s/custom/googleVis/%s",
                    #tools:::httpdPort,
                    ifelse(R.version['svn rev'] < 67550 | getRversion() < "3.2.0",
                           get("httpdPort", envir=environment(startDynamicHelp)),
                           tools::startDynamicHelp(NA)
                           ),
                    basename(file))
     if(interactive()){
#      viewer <- getOption("googleVis.viewer")
#       if (!is.null(viewer) )        
#         viewer(.url)#"http://localhost:8100")
#       else
             browseURL(.url, ...)
     }else{ ## not interactive modus     
      browseURL(.url, browser='false',...)
     }
    invisible(file)
  }else{ ## givs.plot.tag not NULL
    if('gvis' %in% class(x)){
      return(print(x, tag=tag))
    }else{
      return(print(x))
    }
  }  
}
  
gvisMerge <- function(x, y, horizontal=FALSE, tableOptions='border="0"',
                      chartid){

  type="gvisMerge"

  if(any(c(missing(x), missing(y))))
    stop("Please provide two gvis-objects as input parameters.\n")
     
  ## test x and y are givs objects
  if(!any(c(inherits(x, "gvis"),   inherits(y, "gvis"))))
    stop("x and y have to be gvis objects\n")
  
  if(missing(chartid)){   
    chartid <- paste("Merged", basename(tempfile(pattern="")),sep="ID")
  }

  htmlScaffold <- gvisHtmlWrapper(title="", chartid=chartid, dataName="various", type=type)
  
  output <- structure(
                 list(type=type,
                      chartid=chartid, 
                      html=list(header=htmlScaffold[["htmlHeader"]],
                        chart=c(jsHeader=paste(x$html$chart["jsHeader"]),
                          jsData=paste(x$html$chart["jsData"], y$html$chart["jsData"], sep="\n"),
                          jsDrawChart=paste(x$html$chart["jsDrawChart"], y$html$chart["jsDrawChart"], sep="\n"),
                          jsDisplayChart=paste(x$html$chart["jsDisplayChart"], y$html$chart["jsDisplayChart"], sep="\n"),
                          jsFooter=paste(x$html$chart["jsFooter"]),
                          jsChart=paste(x$html$chart["jsChart"], y$html$chart["jsChart"], sep="\n"),
                          divChart= paste("\n<table ", tableOptions, ">\n<tr>\n<td>\n", x$html$chart["divChart"], "\n</td>\n",                            
                            ifelse(horizontal,"<td>\n","</tr>\n<tr>\n<td>\n"), y$html$chart["divChart"],
                            "\n</td>\n</tr>\n</table>\n", sep="")
                          ),
                        caption=htmlScaffold[["htmlCaption"]],
                        footer=htmlScaffold[["htmlFooter"]]
                        )
                      ),
                      class=c("gvis", "list")
                      )
  return(output) 
}


