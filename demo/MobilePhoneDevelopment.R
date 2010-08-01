#In this demo we replicate the analysis from the following blog entry by Richard Heeks
#http://ict4dblog.wordpress.com/2009/11/30/mobile-phone-penetration-google-motion-chart-data-visualisation/

Data=read.csv("http://spreadsheets.google.com/pub?key=tUzZsw5SoG_jXRDl6p8tRCg&single=true&gid=0&output=csv", na.string=c("-", "..."))
head(Data)
## We are only interested in the first six columns
Data <- Data[,c("Country","Year","Mobiles.Per.100.Inhabitants",
                "Population..m..2008.","GDP.per.capita..US...2007.","Geographic")]
## Remove line brakes after names
Data$Country <- gsub("\n","",as.character(Data$Country))
Data$Geographic <- gsub("\n","",as.character(Data$Geographic))
## Lets reorder the columns slightly
Data <- Data[,c(1,2,5,3,4,6)]
## Create motion chart
MotionChartPage(Data, "Country", "Year", file="MobileData.rsp")

