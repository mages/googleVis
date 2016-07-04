## Animated Geo Maps based on ideas and code by:
## Manoj Ananthapadmanabhan and Anand Ramalingam

## Here we animate the percentage of vote for president won by the
## Democratic candidate in the elections from 1932 - 2008

## This demo requires the 'pscl' package
if( !is.element("pscl", installed.packages()[,1]) )
  install.packages("pscl")


require(pscl)
data(presidentialElections)

## Add min and max values to the data
df = data.frame(state=rep(c("Min", "Max"),20), 
		 		demVote=rep(c(0, 100),20),
  		 		year=sort(rep(seq(1932,2008,4),2)))


presidentialElections <- rbind(presidentialElections[,1:3], df)

## Create Geo Maps for each election
gvisData <- by(presidentialElections, list(year=presidentialElections$year), function(x){
	
	year <- x$year[1]	
	g <- gvisGeoChart(x, "state", "demVote", 
		options=list(region="US", dataMode="regions", resolution="provinces"),
		 chartid=paste("[", year, "]", sep=""))
	.data <- g$html$chart["jsData"]
	.data <-gsub("function ", "", .data)
	.data <- sub("\\] ()", "\\] = function ", .data)
	return(.data)	
}
)

animation <- "
var gvisData = {};

var Animation = {};
Animation.startYear = 1;
Animation.endYear = 20;
Animation.currentYear = Animation.startYear;
Animation.divCharts = {};

Animation.playAnimation = function() {
  if (Animation.currentYear > Animation.endYear) {
   	return;
  }
  document.getElementById('chart-header').innerHTML = 'Percent of the vote for president won by the Democratic candidate in year: '+ (1928 + 4*Animation.currentYear);
  if (Animation.currentYear > Animation.startYear) {
  	Animation.divCharts[Animation.currentYear-1].style.display = 'none';
  }
  Animation.divCharts[Animation.currentYear++].style.visibility = 'visible';
  setTimeout(Animation.playAnimation, 2000);
};

"

gvisChart <- '

// jsDrawChart
function drawChart() {
	var chart = {};
	var options ={};
	options["displayMode"] = "regions";
	options["width"] =   600;
	options["region"] = "US";
	options["height"] =  400;
  options["resolution"] = "provinces"
	options["colors"] = ["red", "blue"];
	
	for (var i = Animation.startYear; i<=Animation.endYear; i++) {
	   Animation.divCharts[i] = document.createElement("div");
	   Animation.divCharts[i].className = "pop-chart";
	   document.body.appendChild(Animation.divCharts[i]);
	   chart[i] = new google.visualization.GeoChart(Animation.divCharts[i]);
	   
	   var data = gvisData[(1928+4*i)]();
	   options["title"] = i;
	   chart[i].draw(data,options);
	}
		
	// Animation.playAnimation();
	setTimeout(Animation.playAnimation, 5000);
}

 
// jsDisplayChart 
function displayChart() {
  google.load("visualization", "1", { packages:["geochart"] }); 
  google.setOnLoadCallback(drawChart);
}
// jsChart 
displayChart()

'


htmlHead <- '

<html>
<head>
<title>US Presidential Election by State: 1932 - 2008</title>

<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script type="text/javascript">

'

htmlFoot <-'

</script>
	
<style>
	.pop-chart {
		position: absolute;
		top: 50;
		left: 10;
		display: block;
		visibility: hidden;
	}
</style>
</head>
<body>

<div id="chart-header"></div>

</body>
</html>

'

page <- structure(
                  list(type="AnimatedGeoChart",
                       chartid="presidentialElections",
                       html=list(
                         header=htmlHead,
                         chart=c(animation, gvisData, gvisChart),
                         caption="",
                         footer=htmlFoot)
                       ),
                  class = c("gvis", "list")
                  )
plot(page)


## See demo(package='googleVis') for other available demos.
