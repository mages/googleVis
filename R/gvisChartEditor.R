### File R/gvisChartEditor.R
### Part of the R package googleVis
### Copyright 2010, 2011 Markus Gesmann, Diego de Castillo

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

gvisChartEditor <- function(data, type="Table",options=list(),...){

  ## myCall <- paste("gvis", type, "options=modifyList(list(gvis.editor='Editor'),options),...)", sep="")
  ## eval(parse(text=mvCall)
 
  
  if (type=="Table"){
    
    gvis.object <- gvisTable(data,
         options=modifyList(list(gvis.editor="Editor"),options),...)
  }
  if (type=="LineChart"){
    gvis.object <- gvisLineChart(data,
         options=modifyList(list(gvis.editor="Editor"),options),...)
  }
  gvis.object
}
