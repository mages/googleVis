## This demo shows how country level data can be accessed from
## the World Bank via their API and displayed with a Motion Chart.
## Inspired by Google's Public Data Explorer, see
## http://www.google.com/publicdata/home
##
## For the World Bank Data terms of use see:
## http://data.worldbank.org/summary-terms-of-use
##
## To run this demo an internet connection and Flash are required.
## This demo is part of the googleVis R package.
##
## Markus Gesmann, 24 September 2011
## Distributed under GPL 2 or later


getWorldBankData <- function(id='SP.POP.TOTL', date='1960:2010',
                             value="value", per.page=12000){ 
  require(RJSONIO)
  url <- paste("http://api.worldbank.org/countries/all/indicators/", id,
               "?date=", date, "&format=json&per_page=", per.page,
               sep="")

  wbData <- fromJSON(url)[[2]]
   
  wbData = data.frame(
    year = as.numeric(sapply(wbData, "[[", "date")), 
    value = as.numeric(sapply(wbData, function(x)
      ifelse(is.null(x[["value"]]),NA, x[["value"]]))),  
    country.name = sapply(wbData, function(x) x[["country"]]['value']),
    country.id = sapply(wbData, function(x) x[["country"]]['id'])    
    )

  names(wbData)[2] <- value
  
  return(wbData)
}

getWorldBankCountries <- function(){
  require(RJSONIO)
  wbCountries <-
    fromJSON("http://api.worldbank.org/countries?per_page=12000&format=json") 
  wbCountries <- data.frame(t(sapply(wbCountries[[2]], unlist)))
  wbCountries$longitude <- as.numeric(wbCountries$longitude)
  wbCountries$latitude <- as.numeric(wbCountries$latitude)
  levels(wbCountries$region.value) <- gsub(" \\(all income levels\\)",
                                           "", levels(wbCountries$region.value))
  return(wbCountries)
}

## Create a string 1960:this year, e.g. 1960:2011
years <- paste("1960:", format(Sys.Date(), "%Y"), sep="")

## Fertility rate
fertility.rate <- getWorldBankData(id='SP.DYN.TFRT.IN',
                                   date=years, value="fertility.rate")

## Life Expectancy
life.exp <- getWorldBankData(id='SP.DYN.LE00.IN',  date=years,
                             value="life.expectancy") 

## Population
population <- getWorldBankData(id='SP.POP.TOTL',  date=years,
                               value="population")

## GDP per capita (current US$)
GDP.per.capita <- getWorldBankData(id='NY.GDP.PCAP.CD',
                                   date=years,
                                   value="GDP.per.capita.Current.USD") 

## Merge data sets
wbData <- merge(life.exp, fertility.rate)
wbData <- merge(wbData, population)
wbData <- merge(wbData, GDP.per.capita)

## Get country mappings
wbCountries <- getWorldBankCountries()

## Add regional information
wbData <- merge(wbData, wbCountries[c("iso2Code", "region.value", 
                                      "incomeLevel.value")],
                by.x="country.id", by.y="iso2Code")

## Filter out the aggregates and country id column
subData <- subset(wbData, !region.value %in% "Aggregates" , select=
                  -country.id) 

## Create a motion chart
M <- gvisMotionChart(subData, idvar="country.name", timevar="year",
                     options=list(width=700, height=600))

## Display the chart in your browser
plot(M)
