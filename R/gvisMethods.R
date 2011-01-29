### File R/gvisMethods.R
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

print.gvis <- function(x, file="",...){
  
  cat(unlist(x$html), file=file, ...)

}


isServerRunning <- function() {
  tools:::httpdPort > 0L
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

plot.gvis <- function(x,...){
  
  if(!isServerRunning()) {
    tools:::startDynamicHelp()
  }

  env <- get( ".httpd.handlers.env", asNamespace("tools"))
  env[["googleVis"]] <- googlevis.httpd.handler
  
  root.dir <- tempdir()

  ## Write the pure chart html code into a separate file
  chart.txt <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
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
<p>You can copy and paste the code below into an existing HTML page.</p>
<p><textarea rows="50" name="html" cols="80">
%s
</textarea>
</p>
</body>
</html>
'
  chart.txt <- sprintf(chart.txt, x$chartid,gsub(">","&gt;",gsub("<","&lt;",
                                                                 paste(unlist(x$html$chart), collapse="\n")))
                       )
  cat(chart.txt, file=file.path(root.dir, paste("Chart_", x$chartid, ".html", sep="")))

  ## Write the whole visualisation into a html file
  file <- file.path(root.dir, paste(x$chartid ,".html", sep=""))
 
  print(x, file=file)
  
  .url <- sprintf("http://127.0.0.1:%s/custom/googleVis/%s",
                  tools:::httpdPort,
                  basename(file))
  browseURL(.url,...)
  invisible(file)
}
  
