### File R/gvisMethods.R
### Part of the R package googleVis
### Copyright 2010 Markus Gesmann, Diego de Castillo
### Distributed under GPL 2 or later

### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

print.gvis <- function(x,file="",...){

  cat(paste(unlist(x), "\n", sep=""), file=file, ...)
}

plot.gvis <- function(x,
                      file=paste(tempfile(tmpdir=system.file(file.path("rsp", "myAnalysis"),
                                            package = "googleVis")),".rsp",sep="") ,
                      repos=paste("http://127.0.0.1:8074/",
                        basename(dirname(system.file(package="googleVis"))),
                        "/googleVis/rsp/myAnalysis/", sep=""),
                      ...){
  ## Example
  ## M <- M=gvisMotionChart(Fruits, "Fruit", "Year")
  ## plot(M, file="~/Sites/myFruitAnalysis.html", repos="http://Cognac.local/~Markus/")
  
  print.gvis(x, file, ...)

  browseRsp(paste(repos, basename(file), sep=""))

}
