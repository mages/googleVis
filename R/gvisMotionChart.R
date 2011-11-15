### File R/gvisMotionChart.R
### Part of the R package googleVis
### Copyright 2010, 2011 Markus Gesmann, Diego de Castillo

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


gvisMotionChart <- function(data, idvar="id", timevar="time", date.format="%Y/%m/%d",
                            options=list(), chartid){

  my.type <- "MotionChart"
  dataName <- deparse(substitute(data))

  ## Combine options for other generic functions
  my.options <- list(gvis=modifyList(list(width = 600, height=500), options),
                     dataName=dataName,
                     data=list(idvar=idvar, timevar=timevar,
                       date.format=date.format, allowed=c("number",
                                                  "string", "date"))
                     )
  
  checked.data <- gvisCheckMotionChartData(data, my.options)
   
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid)

  return(output)
}

gvisCheckMotionChartData <- function(data, options){
  
  ## Motion Charts require in the first column the idvar and time var in the second column
  ## The combination of idvar and timevar has to be unique

  ## Google Motion Chart needs a 'string' in the id variable (first column)
  ## A number or date in the time variable (second column)
  ## Everything else has to be a number or string
  
  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  ## typeMotionChart will hold the Google DataTable formats of our data
  typeMotionChart <- as.list(rep(NA, length(varNames)))
  names(typeMotionChart) <- varNames
  
  ## Check if idvar and timevar match columns in the data
  idvar.timevar.pos <- match(c(options$data$idvar, options$data$timevar), varNames)
  if(sum(!is.na(idvar.timevar.pos)) < 2){
    stop("There is a missmatch between the idvar and timevar specified and the colnames of your data.")
  }


  typeMotionChart[[options$data$timevar]] <-
    testTimevar(x[[options$data$timevar]], options$data$date.format)

  if(typeMotionChart[[options$data$timevar]] == "string"){
  ## only true for weekly data
    x[[options$data$timevar]] <- format.Date(x[[options$data$timevar]],
                                             options$data$date.format) 
  }
    
  
  ## idvar has to be a character, so lets try to convert it into a character
  if( ! is.character(x[[options$data$idvar]]) ){
    x[[options$data$idvar]] <- as.character(x[[options$data$idvar]])
  }
  typeMotionChart[[options$data$idvar]] <- "string"
  
  varOthers <- varNames[ -idvar.timevar.pos  ]
  varOrder <- c(options$data$idvar, options$data$timevar, varOthers)
  x <- x[varOrder]
  
  typeMotionChart[varOthers] <- sapply(varOthers, function(.x)
                                       ifelse(is.numeric(x[[.x]]), "number","string"))
  
  typeMotionChart <- typeMotionChart[varOrder]
  x[varOthers] <- lapply(varOthers,function(.x){
    if(class(x[[.x]])=="Date") as.character(x[[.x]]) else x[[.x]]
  }) 
  
  
  ## check uniqueness of rows

  if( nrow(data) != nrow(unique(as.data.frame(x)[1:2]))  ){
    stop("The data must have rows with ",
         "unique combinations of idvar and timevar.\n",
         "Your data has ",
         nrow(data),
         " rows, but idvar and timevar only define ",
         nrow(unique(as.data.frame(x)[1:2])),
         " unique rows.")
  }
  
  return(data.frame(x))
}

testTimevar <- function(x, date.format){
  ## Check if timevar is either a numeric or date
  if( is.numeric(x) )
    return("number")  
  
  if(class(x)=="Date"& date.format %in% c("%YW%W","%YW%U"))
    return("string") 

  if(class(x)=="Date")
    return("date")
  
  stop(paste("The timevar has to be of numeric or Date format. Currently it is ", class(x)))

}
  
