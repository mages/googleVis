require(PerformanceAnalytics)
data(managers)

managerNames <- names(managers)
width <- 36
stepsize <- 12
PerformanceSummary <- lapply( managerNames, function(x){
                             y <- managers[,x,drop=FALSE]
                             out <- data.frame(Manager=x,
                                               risk.column = na.omit(apply.rolling(y[(nrow(y)%%stepsize +
                                               1):nrow(y), 1, drop = FALSE], width = width, FUN = StdDev.annualized,
                                               by = stepsize)),
                                               returns.column = na.omit(apply.rolling(y[(nrow(y)%%stepsize +
                                               1):nrow(y), 1, drop = FALSE], width = width, FUN = Return.annualized,
                                               by = stepsize))
                                               )
                             return(out)
                         })

PerformanceSummary <- na.omit(do.call("rbind", PerformanceSummary))
names(PerformanceSummary) <- c("Manager","StdDev.annualized", "Return.annualized")
PerformanceSummary$RollingYear <- as.numeric(format(as.Date(rownames(PerformanceSummary)), "%Y"))

PerformanceSummary$Category <- ifelse(PerformanceSummary$Manager %in% paste("HAM",1:6, sep=""), "Individual", "Benchmark")

## Google Motion Chart
MotionChartPage(PerformanceSummary, "Manager", "RollingYear", file="PerformanceSummary.rsp",
                options=list(width=600,
                                 height=500)
                )

## compare to a chart.SnailTrail

chart.SnailTrail(managers[,c("HAM2","SP500 TR"),drop=FALSE], width=36, stepsize=12,
                 colorset=c('red','orange'),add.names="firstandlast", rf=.04/12, main="Trailing 36-month Performance Calc'd Every 12 Months")
