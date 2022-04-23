# googleVis 0.7.0

 * New gvisGantt function for Gantt charts
 * Removed Flash based charts from demo

# googleVis 0.6.12

 * Added example to gvisAnnotationChart help file that
   demonstrates how to change the background colour
 * The annotated timeline now automatically uses Annotation Charts instead
   
# googleVis 0.6.11

 * Started using pkgdown to generate googleVis package website
 * Fixed warning from type.convert()
 * Tidied up layout of googleVis vignette

# googleVis 0.6.10

* Allow custom HTML tooltips in gvisGeoCharts
 
# googleVis 0.6.9

* Fixed URLs to comply with CRAN checks

# googleVis 0.6.8

* Fixed URLs to comply with CRAN checks
 
# googleVis 0.6.7

* Removed dependency on package knitcitations

# googleVis 0.6.6

* Changed option in YAML header of vignette to 
   self_contained: false, as otherwise the plots 
   are not displayed
* Re-organised vignettes and use HTML output only

# googleVis 0.6.5

* Updated URLs to Google's online documentation

# googleVis 0.6.4

* added gvisWordTree function. 
   Thanks to Ashley Baldry for contributing the code.

# googleVis 0.6.3

* Corrected URL for gvisCalendar documentation by Google
* Allow for numeric colorvar input for gvisBubbleChart to create 
   a gradient color scale

# googleVis 0.6.2

 * Updated links to omegahat from omegahat.org
    to omegahat.net as requested by CRAN

# googleVis 0.6.1

* Flash based charts gvisGeoMap and gvisAnnotatedTimeLine will 
   show a warning, reminding users to switch to the HTML5 charts
   gvisGeoChart and gvisAnnotationChart instead.
* In some cases option settings were not converted 
   properly into JSON objects.  

# googleVis 0.6.0

 * Changed package dependency from RJSONIO to jsonlite.
 * Changed demo AnimatedGeoMap to AnimatedGeoChart

# googleVis 0.5.10

 * Ordered factors were ignored. 
   Thanks to Carsten Langer for reporting this issue.

# googleVis 0.5.9

  * Added note section on width and height to help file of 
    gvisIntensityMap. Thanks to Sarang Brahme for his comment.
  * Updated NAMESPACE file to comply with new R CMD checks in R-3.3.0 
  
# googleVis 0.5.8

* Internal changes to how the internal web server is called, to reflect
  changes in R 3.2.0

# googleVis 0.5.7

* Updated DESCRPITION file to comply with new CRAN policy
* Clarified setting parameters in help file.
  Thanks to Nick Salkowski for his suggestions.

# googleVis 0.5.6

* Rescaled the column "% of World Population" in sample data 
  set "Population" by a factor of 0.01
* gvisMotionChart: arguments xvar, yvar, sizevar and colorvar were not 
  always picked up correctly. 
  Thanks to Juuso Parkkinen for reporting this issue.

# googleVis 0.5.5

* Added example to gvisMerge help file.
* README.md when converted to (X)HTML using a current version of 
  pandoc showed minor problems when validated using W3C Markup 
  Validator.
* In some case when no xvar and yvar arguments where provided for 
  core charts the output wasn't sensible. This bug was introduced 
  with version 0.5.3. Thanks to stanstrup for reporting this issue.

# googleVis 0.5.4

* Tidying up of googleVis demo, vignette and README file

# googleVis 0.5.3

* Default chart width is set to 'automatic' instead of 500 pixels.
* Intervals for columns roles have to end with the suffix ".i",
  with i being an integer. Several interval columns are allowed,
  see the Roles demo and vignette for more details.
* The order of y-variables in core charts wasn't maintained.
  Thanks to John Taveras for reporting this bug.
* Width and height of googleVis charts were only accepted in pixels, 
  although the Google Charts API uses standard HTML units (for
  example, '100px', '80em', '60', 'automatic'). If no units are specified 
  the number is assumed to be pixels. This has been fixed.
  Thanks to Paul Murrell for reporting this issue.

# googleVis 0.5.2

* Fixed minor formatting issues in documentation and vignettes.
* Added examples in demo googleVis to show how to
  customize points and lines and to the help files of
  gvisLineChart and gvisScatterChart.

# googleVis 0.5.1

* New functions gvisSankey, gvisAnnotationChart, gvisHistogram,
  gvisCalendar and gvisTimeline to support the new Google charts 
  of the same names (without 'gvis').
* New demo Trendlines showing how trend-lines can be added to
  Scatter-, Bar-, Column-, and Line Charts.
* New demo Roles showing how different column roles can be used
  in core charts to highlight data.
* New vignettes written in R Markdown showcasing googleVis
  examples and how the package works with knitr.
* The help files of gvis charts no longer show all their options,
  instead a link to the online Google API documentation is given. 
* Updated googleVis demo
* All googleVis output will be displayed in your default browser. 
  In previous versions of googleVis output could also be displayed 
  in the preview pane of RStudio. This feature is no 
  longer available with the current version of RStudio, but is likely to   
  be introduced again with the release of RStudio version 0.99 or higher.

# googleVis 0.4.7

* New option 'googleVis.viewer' which controls the default output of
  the googleVis plot method. On package load it is set to 
  getOption("viewer"). It you use RStudio, then its viewer pane will 
  be used for displaying non-Flash charts. 
  Set options("googleVis.viewer"=NULL) and the googleVis
  plot function will open all output in the default browser again.
* The package start-up message makes the user aware of the default 
  viewer option.
* Added example to gvisMap that illustrates how the icon can be 
  changed.

# googleVis 0.4.6

* googleVis will use the Viewer pane in RStudio (version >= 0.98.441) 
  to display non-Flash charts by default. The setting is controlled
  via the option viewer. Set options("viewer"=NULL) and the googleVis
  plot function will open all output in the default browser again.

# googleVis 0.4.5

* The indentation of some of the HTML output changed in version 0.4.4,
  which as a result stopped googleVis output to be rendered with knitr.

# googleVis 0.4.4

* gvisTable() gained new parameter formats, which allow users to 
  specify the format of numbers displayed in a table. 
  Thanks to Jacqueline Buros for providing ideas and code.
* Doughnut charts are now possible as pie charts with a hole.
* New examples for gvisBarChart, gvisColumnChart, gvisComboChart 
  demonstrating how to change the width of bars
* Extended FAQ section

# googleVis 0.4.3

 * givsGeoChart has a new argument 'hovervar' to specify a column in
   input data that can be used to show additional information in a geo
   chart. See the new example of plotting countries' credit rating in
   the help file for a use case. Thanks to John Muschelli for suggesting
   this feature.

# googleVis 0.4.2

 * The core charts (e.g. line, area, bar, column and combo charts)
   accept now also date variables for the x-axis. Thanks to Sebastian
   Campbell for pointing this out.
 * The WorldBank demo uses now the WDI package.
   Thanks to John Maindonald for providing the code.
 * Fixed typos in Stock and Andrew example data. 
   Thanks to Sebastian Campbell for reporting this issue. 

# googleVis 0.4.0

 * New function renderGvis to support shiny.
   This function allows user to insert googleVis output into shiny
   apps, similar to renderText and renderPlot. See the help page for
   more details. Many thanks to Joe Cheng for his support and help.
 * In order to support shiny the order of the elements of the
   gvis*()$html$chart vector changed. The positions of jsChart and
   jsFooter have been swapped.
 * The load mechanism for the Google API changed from http to https
   again. Thanks to Jacques Philip. 
 * The package dependencies changed to imports statements in DESCRIPTION. 
   Thanks to Suraj Gupta for pointing this out.
 * The R.rsp example in demo googleVis has been moved into its own
   demo Rrsp. 
 * A FAQ and shiny section has been added to the vignette.
 * jsDisplayChart didn't check if the google visualization function is already
   loaded. Many thanks to Mark Melling for reporting the issue and
   providing a solution.
 * The demo WorldBank didn't download all data but only the first
   12000 records. Many thanks to John Maindonald reported this issue. 

# googleVis 0.3.3

 * Clarified the usage of the argument state in the help file of
   gvisMotionChart. Thanks to Leonardo Trabuco 

# googleVis 0.3.3

 * plot.gvis didn't open a browser window when options(gvis.plot.tag)
   was not set to NULL, but the user explicitly called plot.gvis with
   tag NULL. Thanks to Sebastian Kranz for reporting this bug. 

# googleVis 0.3.2

 * plot.gvis gained the argument 'tag', which works similar to the
   argument of the same name in print.gvis. By default the tag
   argument is NULL and plot.gvis has the same behaviour as in the
   previous versions of googleVis. 
   However, the argument can be set outside plot.gvis via
   options(gvis.plot.tag=...). This allows users to switch plot
   statements into print statements by changing only one setting. This
   is particular useful when googleVis is used in combination with
   knitr or R.rsp. Setting options(plot.gvis.tag="chart") will ensure
   that plot(gvisOutput) statements will be included into the final
   HTML output. See the help file to plot.gvis and vignette for more
   details. 
 * The tag argument of print.gvis can be set globally from outside the
   function via options(gvis.print.tag)
 * The vignette has a new section describing how to set the
   behaviour of plot.gvis and print.gvis via options(gvis.plot.tag),
   options(gvis.print.tag) respectively. The section describing how
   googleVis can be used with knitr has been extended and an additional
   example included. 
 * plot.gvis can open any html file now, not just gvis-objects. Like
   with gvis-object it will copy the file into a temporary directory
   and display it via the R HTTP server. 

# googleVis 0.3.1

* The argument 'browser' introduced in version 0.3.0 has been removed
  again. The argument was set by default to the output of
  getOptions('browser'), if interactive() returned TRUE, otherwise to
  'false'. The function getOptions('browser') returns either a string
  or a function call. The later caused an error message, as
  experienced with RStudio and RGui.exe. The check is now handled
  internally by plot.gvis.
  Thanks to Sebastian Kranz for reporting this bug.

# googleVis 0.3.0

* plot.gvis has a new argument 'browser'. The argument is passed on
  to the function browseURL. The 'browser' argument is by default set
  based on the output of interactive(). This prevents R CMD CHECK
  trying to open browser windows during the package checking
  process. See the help file of plot.gvis for more details.
  Thanks to Henrik Bengtsson for his comments and suggestions.
* gvisMotionChart has new arguments xvar, yvar, colorvar and
  sizevar. Those arguments are optional and set the various dimensions 
  of a motion chart, similar to those in gvisBubbleChart.
  Thanks to Sebastian Kranz for the idea and initial code.
* gvisGeoChart accepts tooltip.triggers following an update of the
  Visualisation API by Google, 24 September 2012
* R data frames are transformed into JSON objects using a new function 
  provided by Sebastian Kranz and Wei Luo. The new function speeds up 
  the googleVis functions.
* Changed the load mechanism for the Google API from http to https.
  Thanks to Erik Bülow for pointing this out (Issue 19). 
* Changed example in help file of gvisMap to show how to include html
  code in tooltip. 
* Chart editor was not validated properly under XHMTL 1.0 Strict

# googleVis 0.2.17

 * Added sections with information to 'knitr' and 'Rook' to vignette 
 * Added example to gvisMerge demonstrating the use of 'Reduce'
 * Data frames with one row only were not displayed in a chart.
   Thanks to Oliver Jay and Wai Tung Ho for reporting this issue.
 * Fixed earth quake example, using data from 
   https://ds.iris.edu/seismon/eventlist/index.phtml, 
   Mag was read as factor rather than numeric

# googleVis 0.2.16

 * Updated example in help file of gvisGeoChart for individual colour
   axis 
 * Updated links to Google API pages
 * gvisMotionCharts accepts quarterly and weekly time dimensions.
   Thanks to Jason Pickering for providing a patch. 

# googleVis 0.2.15

*  Updated documentation following a new version of the Google API
   on 22 February 2012. 
*  Moved vignette from folder /inst/doc to /vignettes
*  Quoted from Google
   https://developers.google.com/chart/interactive/docs/release_notes?csw=1: 
   - Added gradient color mode to bubble chart.
   - Geo chart:
     *  Region interactivity in marker mode is now disabled by
     	default. How to keep the old behavior? Set the
   	enableRegionInteractivity option to true.
     *  Markers are now opaque by default. How to keep the old
        behaviour? Set the markerOpacity option to 0.5.
     *  Marker size is now between 3 and 12 pixels by default. How to
        keep the old behavior? Set the sizeAxis option to {minSize: 2,
        maxSize: 30}.
     *  A magnifying glass is now opened when the user hovers over
        cluttered markers (excluding IE<=8). How to keep the old
        behaviour? Set the magnifyingGlass option to {enable: false}.
     *  Maps are not stretched by default anymore, but rather kept at
      	the original aspect ratio. How to keep the old behavior? Set the
  	keepAspectRatio option to false.

# googleVis 0.2.14

*  Updated help files to be in line with the Google Visualisation
   API pages
*  Updated vignette with new section on dealing with apostrophes in
   column names and updated example in section "Setting options"
*  Fixed typos in vignette. Thanks to Pat Burns for pointing them
   out
*  Updated links to Google's updated API Terms of Use:
   https://developers.google.com/terms/site-terms  
*  New bubble chart function gvisBubbleChart

# googleVis 0.2.13

 *  The list of arguments for gvisGeoChart changed:
    - the argument 'numvar' has been renamed to 'colorvar' to
      reflect the updated Google API. Additionally gvisGeoChart
      gained a new argument 'sizevar'.
 *  Updated googleVis vignette with a section on using googleVis 
    output in presentations  
 *  Renamed demo EventListner to EventListener
 *  Google published a new version of their Visualisation API on 7
    December 2011. Some of the new features have been implemented
    into googleVis already:
    - New stepped area chart function gvisSteppedAreaChart
    - gvisGeoChart has a new marker mode, similar to the mode in
      gvisGeoMap. See example(gvisGeoChart) for the new
      functionalities.

# googleVis 0.2.12

 *  gvisMotionChart didn't display data with special characters,
    e.g. spaces, &, %, in column names correctly. 
    Thanks to Alexander Holcroft for reporting this issue.

# googleVis 0.2.11

*  Updated vignette and documentation with instructions on changing
   the Flash security settings to display Flash charts locally. 
   Thanks to Tony Breyal.
*  New example to plot weekly data with gvisMotionChart
*  Removed local copies of gadget files to reduce package file
   size. A local copy of the R script to generate the original gadget
   files is still included in inst/gadgets 

# googleVis 0.2.10

*  Updated section 'Using googleVis output with Google Sites,
   Blogger, etc.' vignette
*  Updated example for gvisMotionChart, showing how the initial
   chart setting can be changed, e.g to display a line chart.
*  New example for gvisAnnotatedTimeLine, showing how to shade
   areas. Thanks to Mike Silberbauer for providing the initial code.    
*  New demo WorldBank. It demonstrates how country level data can
   be accessed from the World Bank via their API and displayed with a
   Motion Chart. Inspired by Google's Public Data Explorer, see
   https://www.google.com/publicdata/home
  
# googleVis 0.2.9

*  The documentation of googleVis has been update to reflect a new
   version of the Google Visualisation API which was published on 
   13 July, see
   https://developers.google.com/chart/interactive/docs/release_notes?csw=1#july-13-2011. 
   Here are some of the most interesting features:   
   - Support for dual Y axes
   - Ability to crop and zoom chart area to specific ranges
   - Ability to set different properties for each series
   - Ability to enable or disable chart interactivity
   - Performance improvements in GeoChart
*  Updated vignette with new sections on
   - Setting options
   - How to use the on-page chart editor
   - Using googleVis with other Google products such as
     Blogger and Google Sites 
*  Updated warning section for gvisTreeMap
*  New gvis.editor argument in options, which adds an edit
   button to the page, allowing the user to edit, change and
   customise the chart on the fly.

# googleVis 0.2.8

*  Updated package welcome message. The message asks the user to read Google's
   Visualisation and Maps API Terms of Use before she uses the functions of the
   googleVis package. 
*  The caption gvis-plots contain an additional link to Google's data policy.
*  New example for gvisBarChart using the XML package to chart online data from Wikipedia

# googleVis 0.2.7

 *  The vignette includes new sections describing:
    - how output of the googleVis package can be included into
      WordPress pages,  
    - how JavaScript event handlers can be added to charts. 
 *  Clarified documentation for Flash based charts in help files of 
    motion chart, geo map, annotated time line. 
 *  New demo 'EventListener' showcasing how a 'Listener' event can be
    added to charts  
 *  gvisGeoMap documentation stated that the default dataMode is
    'regions', but the function actually used 'markers'. The default
    for dataMode is now 'regions' and therefore in line with the
    help file.   

# googleVis 0.2.6

*  Updated demos
*  New interfaces to three more interactive Google charts:
   - gvisComboChart
   - gvisGeoChart
   - gvisCandlestickChart
*  New function 'gvisMerge' to align two charts next to each other

# googleVis 0.2.5

*  New interfaces to more interactive Google charts:
   - gvisLineChart
   - gvisBarChart
   - gvisColumnChart
   - gvisAreaChart
   - gvisScatterChart
   - gvisPieChart
   - gvisGauge
   - gvisOrgChart
   - gvisIntensityMap 
*  New demo 'AnimatedGeoMap'. The demo shows how a Geo Map can be animated
   with additional JavaScript. 
   Thanks to Manoj Ananthapadmanabhan and Anand Ramalingam, who
   provided the idea and initial code.
*  The way RJSONIO treats backslashes changed in version 0.7.1 and
   as a result some Google charts were no longer displayed.

# googleVis 0.2.4

*  plot.gvis no longer writes into the package folder. Instead
   temporary files are created. This overcomes the need to install
   the package into a directory with write access. Many thanks to
   Ben Bolker for this suggestion and code contribution.  
*  plot.gvis no longer requires the web server provided by
   the R.rsp package to display the visualisation output. Instead it
   uses the internal R HTTP help server. Many thanks to John Verzani
   for this suggestion and code contribution. 
*  R >= 2.11.0 is required to plot googleVis output, as it uses the
   internal R HTTP help server.
*  Updated vignette with a section on how to use googleVis with
   RApache and brew
*  The plot function generates a web page which includes a link
   to the HTML code of the chart. Many thanks to Henrik Bengtsson
   for this suggestion.
*  gvis visualisation functions have a new argument 'chart id', to
   set the chart id of the exhibit manually. 	         
*  gvis functions return more details about the visualisation chart
   in a structured way. Suppose x is a 'gvis' object, than
   x$html$chart is a named character vector of the chart's
   JavaScript building blocks and html tags. 
*  print.gvis has a new argument 'tag', which gives the user more
   control over the output
*  Brew example files in: 
   system.file("brew", package = "googleVis")  
*  gvisTable did not accept datetime columns.

# googleVis 0.2.3

*  gvisAnnotatedTimeLine accepts date in POSIX* formats
*  Google date objects expect the months Jan.- Dec. as 0 - 11 and
   not 1 - 12 
*  Fixed typo in the Andrew data set. The Pressure at 1992-08-24
   12:00:00 was 951 and not 51  

# googleVis 0.2.2

    
 *  Fixed typos in documentation
 *  New function:
     - createGoogleGadget which allows users to create Google Gadgets XML output  

# googleVis 0.2.1

    
 *  First version to be released on CRAN
 *  New function:
    - gvisAnnotatedTimeLine to generate interactive annotated time
     line charts 

# googleVis 0.2.0

*  The package has been renamed from GoogleMotionChart to googleVis 
   to reflect a new more flexible implementation.
*  More functions of the Google Visualisation API are now available.
*  New interfaces, all visualisation functions start with 'gvis'.
*  Output is now of class 'gvis' with generic print and plot
   functions.
*  'gvis' objects are list of lists, which allow the user to extract
   the various parts of the visualisation output, e.g. the chart
   object.
*  New functions:
   - gvisMotionChart to generate motion charts
   - gvisGeoMap to generate geographical maps
   - gvisMap to generate maps
   - gvisTreeMap to generate tree maps
   - gvisTable to generate table output
   - print.gvis: generic function to print 'gvis' objects
   - plot.gvis: generic function to display 'gvis' objects in a
     browser via the R.rsp package.  

# googleVis 0.1.4

*  The package uses the RJSONIO package from Omegahat to
   transform a data.frame into a json DataTable 

# googleVis 0.1.3

*  More detailed motion chart configuration settings are possible.
*  options have to be set via a list. Arguments height and width
   can be set, plus further configurations.
*  Updated demo PerformanceAnalyticsMotionChart   

# googleVis 0.1.2

* First public version.
