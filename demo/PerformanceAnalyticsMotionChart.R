require(PerformanceAnalytics)
data(managers)
managerNames <- names(managers)

PerformanceSummary <- lapply( managerNames, function(x)
                         data.frame(Manager=x,
                                    apply.rolling(managers[,x,drop=FALSE], FUN="sd", width=36),
                                    apply.rolling(managers[,x,drop=FALSE], FUN="mean", width=36))
                         )

PerformanceSummary <- na.omit(do.call("rbind", PerformanceSummary))
names(PerformanceSummary) <- c("Manager","Std", "Mean")
PerformanceSummary$Rolling36Dates <- as.Date(rownames(PerformanceSummary))
PerformanceSummary$Category <- ifelse(PerformanceSummary$Manager %in% paste("HAM",1:6, sep=""), "Individual", "Benchmark")

## Traditional lattice plot
require(lattice)
xyplot(Mean + I(Mean+Std) +I(Mean-Std) ~ Rolling36Dates | Manager,
       data=PerformanceSummary, t="l", col=c(1,2,2), ylab="Mean (+/-)Std",
       as.table=TRUE,
       panel=function(x,y,...){
           panel.grid(v=-1, h=-1)
           panel.abline(h=0, lty=3)
           panel.xyplot(x,y,...)
           })

## Google Motion Chart
MotionChartPage(PerformanceSummary, "Manager", "Rolling36Dates", file="PerformanceSummary.rsp")
