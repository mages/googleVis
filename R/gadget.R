createGoogleGadget <- function(gvis){

  if(!"gvis" %in% class(gvis)){
    stop("A gvis object is as input required.")
  }
sprintf('<?xml version="1.0" encoding="UTF-8" ?> 
<Module>
  <ModulePrefs title="%s" />
  <Content type="html">
  <![CDATA[ 
   %s
  ]]> 
  </Content>
</Module>
',  gvis$chartid, paste(gvis$html$chart, collapse="\n") ) 
  
}
