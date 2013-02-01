reactiveGvis <- function(func) {
  function() {
    chart <- func()
    paste(chart$html$chart, collapse="\n")
  }
}
