# Add the ability to pass columns roles for further
# processing downstream
#
# Thanks to Oliver Gjoneski
#
# Role columns must follow column they pertain to.  Proper naming
# conventions must be be observed.  For example, roles fulfilling tooltip
# roles and must be called "foo.blah.tooltip".
#
# For more details see:
# https://developers.google.com/chart/interactive/docs/roles

## ---- Tooltip ----
df <- data.frame(year=1:11,pop=1:11,
                 pop.html.tooltip=letters[1:11])
plot( 
  gvisScatterChart(df)
)

## ---- TooltipHTML ----
levels(df$pop.html.tooltip)[1] <- 
  '<a href="http://www.r-project.com"><img src="http://www.r-project.org/Rlogo.jpg" alt="R logo" /></a>'
plot( 
  gvisScatterChart(df,
                   options=list(tooltip="{isHtml:'true'}"))
)

## ---- CertaintyScopeEmphasis ----
df <- data.frame(year=1:11, x=1:11,
                 x.scope=c(rep(TRUE, 8), rep(FALSE, 3)),
                 y=11:1, y.html.tooltip=LETTERS[11:1],                 
                 y.certainty=c(rep(TRUE, 5), rep(FALSE, 6)),
                 y.emphasis=c(rep(FALSE, 4), rep(TRUE, 7)))
plot(
  gvisScatterChart(df,options=list(lineWidth=2))
)

## ---- ColumnChart ----
dat <- data.frame(Year=2010:2013,
                  Sales=c(600, 1500, 800, 1000),
                  Sales.html.tooltip=c('$600K in our first year!',
                                       'Sunspot activity made this our best year ever!',
                                       '$800K in 2012.',
                                       '$1M in sales last year.'),
                  Sales.certainty=c(TRUE, FALSE, TRUE, FALSE))
plot(
  gvisColumnChart(dat, xvar='Year', 
                  yvar=c('Sales', 'Sales.certainty')
  )
)

## ---- Interval ----
df <- data.frame(Year=2013:2014, Sales=c(120, 130), 
                 Sales.interval=c(100,110), 
                 Sales.interval=c(140, 150),
                 Sales.style=c('red', 'gold'),
                 Sales.annotation=c("North", "South"),
                 check.names=FALSE)

plot(
  gvisBarChart(df, xvar='Year', 
               yvar=c('Sales', 
                      'Sales.style',
                      'Sales.annotation',
                      'Sales.interval', 
                      'Sales.interval')
  )
)

plot(
  gvisLineChart(df, xvar='Year', 
               yvar=c('Sales', 
                      'Sales.interval', 
                      'Sales.interval'),
               options=list(series="[{color:'purple'}]")
  )
)


## ---- LimitData ----
plot(
  gvisLineChart(dat, xvar='Year', 
                yvar=c('Sales', 'Sales.emphasis', 
                       'Sales.html.tooltip')
  )
)

## ---- TwoLines ----
df <- data.frame(country=c("US", "GB", "BR"), 
                 val1=c(1,3,4), 
                 val1.emphasis=c(TRUE, TRUE, FALSE),
                 val2=c(23,12,32))
plot(
  gvisLineChart(df, xvar="country", 
                yvar=c("val1", "val1.emphasis")
  )
)

plot(
  gvisLineChart(df, xvar="country", 
                yvar=c("val1", "val1.emphasis", "val2")
  )
)

## ---- VerticalReferenceLine ----
dat <- data.frame(Year=2010:2013, 
                  Sales=c(600, 1500, 800, 1000),
                  Sales.annotation=c('First year', NA, NA, 'Last Year'),
                  Sales.annotationText=c('$600K in our first year!',
                                       NA,
                                       NA,
                                       '$1M in sales last year.'))      

plot(
  gvisLineChart(dat, xvar='Year', 
                  yvar=c('Sales',
                         'Sales.annotation',
                         'Sales.annotationText'),
                         options=list(annotations = "{style:'line'}")
  )
)
