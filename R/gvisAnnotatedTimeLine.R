### File R/gvisMap.R
### Part of the R package googleVis
### Copyright 2010 Markus Gesmann, Diego de Castillo

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

gvisAnnotatedTimeLine <- function(data, datevar="", 
                                  numvar="", idvar="", titlevar="", 
                                  annotationvar="", 
                                  date.format="%Y/%m/%d",
                                  options=list()){
  
  
  my.type <- "AnnotatedTimeLine"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(width = 600, height=300), options), 
		     dataName=dataName,                     
                     data=list(datevar=datevar, numvar=numvar,
                       idvar=idvar, titlevar=titlevar, annotationvar=annotationvar,
		       date.format=date.format,
                       allowed=c("number", "string", "date"))
                     )
  
  checked.data <- gvisCheckAnnotatedTimeLineData(data, my.options,
                                                 idvar=idvar, titlevar=titlevar, annotationvar=annotationvar)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options)
  
  return(output)
}


gvisCheckAnnotatedTimeLineData <- function(data, options, idvar,
                                           titlevar, annotationvar){
  
  data.structure <- list(
                         datevar  = list(mode="required",FUN=check.date),
                         numvar   =  list(mode="required",FUN=check.num),
                         idvar  = list(mode="optional",FUN=check.char),                   
                         titlevar = list(mode="optional",FUN=check.char),
                         annotationvar  = list(mode="optional",FUN=check.char))
  
  x <- gvisCheckData(data=data, options=options, data.structure=data.structure)
   

  x.df <- as.data.frame(x)


    ##check if idvar is missing that datevar define unique rows
  if(idvar==""){
    if(length(x.df[,1]) != length(unique(x.df[,1])))
      message("Warning: The data appears to more than one entry for the same date.\n",
              "Have you considered using the idvar variable?")
    ## if idvar is missing no reshape is required.
    return(x.df)
  }
  

  groups <- factor(x.df[[idvar]])
  ngroups <- nlevels(groups)


  checkTitleAnno <- c(titlevar != "", annotationvar != "")
  if(all(checkTitleAnno)){
    varying.vars <- c(2,4,5)
  }else{
    if(any(checkTitleAnno))
      varying.vars <-  c(2,4)
    else
      varying.vars <- 2
  }
  var.names <- names(x.df)[varying.vars]
  
  x.df <- reshape(x.df,
                  v.names=var.names, ##numvar , titlevar, annotationvar
                  idvar=names(x.df)[1], ## datevar 
                  timevar=names(x.df)[3], ## idvar
                  direction="wide") 

 ## names(x.df)[c(2:(ngroups+1))] <- levels(groups)

  names(x.df) <- gsub(paste(var.names[1], ".", sep=""), "", names(x.df))
  
  return(x.df)
}

