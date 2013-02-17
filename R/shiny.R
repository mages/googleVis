renderGvis <- function(expr, env=parent.frame(), quoted=FALSE) {
  # Convert expr to a function
  func <- shiny::exprToFunction(expr, env, quoted)

  function() {
    chart <- func()
    paste(chart$html$chart, collapse="\n")
  }
}
