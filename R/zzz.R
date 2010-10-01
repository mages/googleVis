.onLoad<- function(lib, pkg, ...)
{
    setMethod("toJSON", "Date",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              tmp <- format(as.Date(x),"new Date(%Y,%m,%d)")
              paste(tmp, collapse=", ")
           })

    require(utils)
    cat("\n",paste("googleVis version", packageDescription("googleVis")$Version,
                   "by:\nMarkus Gesmann <markus.gesmann@gmail.com> and Diego de Castillo <decastillo@gmail.com>\n\n"),
        "Type ?MotionChart to see overall documentation and\n",
        "vignette('googleVis') to see the package vignette,\n",
        "which givers you an overview of the graphical user interface of a motion chart.\n\n",
        "See demo(package='googleVis') for a list of demos.\n\n",
        "Feel free to send me an email if you would like to keep ",
        "informed of\nnew versions or if you have any feedback, ",
        "ideas, suggestions or ",
        "would\nlike to collaborate.\n\n",
        "More information is available on the googleVis project web-site:\n",
        "http://code.google.com/p/google-motion-charts-with-r/\n\n",
        sep='')
  invisible()
}
