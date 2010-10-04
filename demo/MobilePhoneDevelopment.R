#In this demo we replicate the analysis from the following blog entry by Richard Heeks
#http://ict4dblog.wordpress.com/2009/11/30/mobile-phone-penetration-google-motion-chart-data-visualisation/
if(getRversion()>="2.11.0"){ ## prior to version 2.10.1 there was a bug in read.csv which has trouble with this data set
    Data=read.csv("http://spreadsheets.google.com/pub?key=tUzZsw5SoG_jXRDl6p8tRCg&single=true&gid=0&output=csv", na.strings=c("-", "..."))
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
    M <- gvisMotionChart(Data, "Country", "Year")
    M
    plot(M)
}else{
print("Unfortunately this demo works only on R version 2.11.0 and higher.")
}
