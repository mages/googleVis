### File R/shiny.R
### Part of the R package googleVis

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

#' renderGvis
#' 
#' Use a googleVis Chart as Shiny Output
#'
#' @description 
#' This function lets you use googleVis charts as Shiny output. 
#' Use it to wrap a googleVis-generating function that you assign to an 
#' \code{output} element in \code{server.R}; then create an \code{htmlOutput} 
#' with the same name in \code{ui.R}.
#' 
#' @param expr An expression that returns a gvis object.
#' @param env The environment in which to evaluate \code{expr}
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})?
#' This is useful if you want to save an expression in a variable.
#' 
#' @section Details:
#' More information about shiny is available online: 
#'  \url{https://shiny.rstudio.com/}. 
#'  You find further examples with googleVis on shiny on mages' blog:
#'  \url{https://magesblog.com/tags/shiny/}
#'  
#' @return Returns a function that can be assigned to a Shiny \code{output} 
#' element.  
#' 
#' @author Joe Cheng, \email{joe@@rstudio.com}
#' 
#' @keywords shiny 
#' 
#' @examples   
#' \dontrun{
#' # To run this example:
#' shiny::runApp(system.file("shiny/", package="googleVis"))
#' # server.R
#' library(googleVis)
#' 
#' shinyServer(function(input, output) {
#'   datasetInput <- reactive({
#'     switch(input$dataset,
#'            "rock" = rock,
#'            "pressure" = pressure,
#'            "cars" = cars)
#'   })
#'   
#'   output$view <- renderGvis({
#'     gvisScatterChart(datasetInput(),
#'                      options=list(title=paste('Data:',input$dataset)))
#'   })
#' })
#' 
#' # ui.R
#' shinyUI(pageWithSidebar(
#'   headerPanel("googleVis on Shiny"),
#'   sidebarPanel(
#'     selectInput("dataset", "Choose a dataset:",
#'                 choices = c("rock", "pressure", "cars"))
#'   ),
#'   mainPanel(
#'     htmlOutput("view")
#'   )
#' ))
#' }
#' 
#' 
renderGvis <- function(expr, env=parent.frame(), quoted=FALSE) {
  # Convert expr to a function
  func <- shiny::exprToFunction(expr, env, quoted)

  function() {
    chart <- func()
    paste(chart$html$chart, collapse="\n")
  }
}
