# googleVis 
[![Travis-CI Build Status](https://travis-ci.org/mages/googleVis.svg?branch=master)](https://travis-ci.org/mages/googleVis)   [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/googleVis)](https://cran.r-project.org/package=googleVis/) ![downloads](https://cranlogs.r-pkg.org/badges/grand-total/googleVis)


The googleVis package provides an interface between R and the [Google's charts tools](https://developers.google.com/chart/). 
It allows users to create web pages with interactive charts based on R data frames. Charts are displayed locally via the R HTTP help server. 
A modern browser with Internet connection is required and for some 
charts a Flash player. The data remains local and is not uploaded to 
Google.

You find [examples](https://CRAN.R-project.org/package=googleVis/vignettes/googleVis_examples.html) of all googleVis function on [CRAN](https://cran.r-project.org/package=googleVis/). Perhaps the best known example is the [motion chart](https://mages.github.io/googleVis/WorldBank.html), popularised by [Hans Rosling](https://www.gapminder.org) in his [2006 TED talk](https://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen). 

Please read [Google's Terms of Use](https://developers.google.com/terms/) before you start using the package. 

## Installation

You can install the stable version from
[CRAN](https://cran.r-project.org/package=googleVis/):

```s
install.packages('googleVis')
```

To install the current development version from github you need the [devtools package](https://cran.r-project.org/package=devtools/) and the other packages on which googleVis depends:

```s
install.packages(c("devtools","jsonlite", "knitr", "shiny", "httpuv"))
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

See the googleVis package [vignettes](https://cran.r-project.org/package=googleVis/) for more details. For a brief introduction read the five page [R Journal article](https://journal.r-project.org/archive/2011-2/RJournal_2011-2_Gesmann+de~Castillo.pdf) and go through our [tutorial](https://decastillo.github.io/googleVis_Tutorial). More examples have been posted on Markus' [blog](https://magesblog.com/tags/googlevis/).

[<img src="https://raw.github.com/mages/googleVis/master/vignettes/figures/googleVisTutorial2013.png" alt="Tutorial" width="400"/>](https://decastillo.github.io/googleVis_Tutorial)

[![Donate](https://www.paypal.com/en_GB/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=HHPMW8TXCCRSC&lc=GB&item_name=googleVis&currency_code=GBP&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted)

## License

This package is free and open source software, licensed under [GPL 2 or later](https://opensource.org/licenses/gpl-license).

<a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/deed.en_GB"><img alt="Creative Commons Licence" class="c1" src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br />
<span>googleVis documentation</span> by <a href="https://github.com/mages/googleVis" rel="cc:attributionURL">Markus Gesmann &amp; Diego de Castillo</a> is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/deed.en_GB">Creative Commons Attribution-ShareAlike 4.0 International License</a>. Based on a work at <a href="https://developers.google.com/chart/interactive/docs/gallery" rel="dct:source">https://developers.google.com/chart/interactive/docs/gallery</a>.
