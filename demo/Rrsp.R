## See how googleVis functions can be integrated into rsp-files:

## This demo requires the 'R.rsp' package
if( !is.element("R.rsp", installed.packages()[,1]) )
  install.packages("R.rsp")

if(require(R.rsp))
  browseRsp() ## Click on googleVis in the Package section.

## See demo(package='googleVis') for other available demos.
