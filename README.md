# googleVis 

The googleVis package provides an interface between R and the [Google Charts API](https://developers.google.com/chart/interactive/docs/gallery). 

It allows users to create web pages with interactive charts based on R data frames, using the Google Charts API and to display them either via the local R HTTP help server or within their own
sites, without uploading the data to Google. A modern browser with Internet connection is required and for some charts Flash. 

You find [examples](http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html) of all googleVis function on [CRAN](http://cran.r-project.org/web/packages/googleVis/). Perhaps the best known example of the Google Chart API is the motion chart, popularised by [Hans Rosling](http://www.gapminder.org) in his [2006 TED talk](http://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen). 

<iframe src="http://mages.github.io/googleVis/WorldBank.html" width=560 height=560 frameborder=0></iframe>

Please read the [Google API Terms of Use](https://developers.google.com/terms/) before you start using the package. 


## Installation

You can install the stable version from
[CRAN](http://cran.r-project.org/package=googleVis):

```s
install.packages('googleVis')
```

To install the current development version from github you need the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html) and the other packages on which googleVis depends:

```s
install.packages(c("devtools","RJSONIO", "knitr", "shiny", "httpuv"))
```

To install googleVis run:
<!--
Fix my broken path to pdflatex on R
```s
Sys.setenv(PATH=paste(Sys.getenv("PATH"),"/usr/texbin",sep=":"))
```
-->
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

See the googleVis package [vignettes](http://cran.r-project.org/web/packages/googleVis/) for more details. For a brief introduction read the five page [R Journal article](http://google-motion-charts-with-r.googlecode.com/files/RJournal_2011-2_Gesmann%2Bde%7ECastillo.pdf) and go through our [tutorial](http://decastillo.github.io/googleVis_Tutorial). More examples have been posted on Markus' [blog](http://lamages.blogspot.co.uk/search/label/googleVis).

[<img src="https://raw.github.com/mages/googleVis/master/vignettes/figures/googleVisTutorial2013.png" width=400/>](http://decastillo.github.io/googleVis_Tutorial)

[![Donate](https://www.paypal.com/en_GB/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=HHPMW8TXCCRSC&lc=GB&item_name=googleVis&currency_code=GBP&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted)

## License

This package is free and open source software, licensed under [GPL 2 or later](http://opensource.org/licenses/gpl-license).

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/deed.en_GB"><img alt="Creative Commons Licence" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">googleVis documentation</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/mages/googleVis" property="cc:attributionName" rel="cc:attributionURL">Markus Gesmann & Diego de Castillo</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/deed.en_GB">Creative Commons Attribution-ShareAlike 4.0 International License</a>. Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="https://developers.google.com/chart/interactive/docs/gallery" rel="dct:source">https://developers.google.com/chart/interactive/docs/gallery</a>.