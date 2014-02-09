# googleVis 

The googleVis package provides an interface between R and the [Google Chart Tools API](https://developers.google.com/chart/interactive/docs/gallery). 

It allows users to create web pages with interactive charts based on R data frames, using the Google Chart Tools and to display them either via the local R HTTP help server or within their own
sites, without uploading the data to Google. A modern browser with Internet connection is required and for some charts Flash. 

Please visit the [project web site](https://code.google.com/p/google-motion-charts-with-r/) for more information and examples and 
read the [Google API Terms of Use](https://developers.google.com/terms/) before you use the package.

## Installation

You can install the stable version from
[CRAN](http://cran.r-project.org/package=googleVis):

```s
install.packages('googleVis', dependencies = TRUE)
```

To install the current development version from github you need the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html):

```s
install.packages("devtools")
```

To install googleVis run:

```s
library(devtools)
install_github("mages/googleVis")
```

## Usage

```s
library(googleVis)
?googleVis
demo(googleVis)
```

See the googleVis package [vignettes](http://cran.r-project.org/web/packages/googleVis/) for more details. For a brief introduction read the five page [R Journal article](http://google-motion-charts-with-r.googlecode.com/files/RJournal_2011-2_Gesmann%2Bde%7ECastillo.pdf) and go through our [tutorial](http://decastillo.github.io/googleVis_Tutorial). More examples and post have been posted on Markus' [blog](http://lamages.blogspot.co.uk/search/label/googleVis).

[<img src="https://raw.github.com/mages/googleVis/master/vignettes/figures/googleVisTutorial2013.png" width=400/>](http://decastillo.github.io/googleVis_Tutorial)