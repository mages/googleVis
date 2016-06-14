### File R/repr.R
### Part of the R package googleVis

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

#' repr_html.gvis
#'
#' Output a gvis chart as an embedded html chart in the Jupyter ecosystem
#'
#' @description 
#' This function lets you use googleVis charts as html output in e.g. a Jupyter Notebook. 
#'
#' @param obj A gvis object.
#' @param ... unused
#'
#' @return Returns the html representation of the gvis object as string
#'
#' @author Jan Schulz, \email{jasc@gmx.net}
#'
#' @name repr_*.gvis
#'
#' @keywords jupyter 
#'
#' @export
repr_html.gvis <- function(obj, ...){
  htmlfile <- tempfile(fileext = '.html')
  on.exit(unlink(htmlfile))
  print(obj, tag = 'chart', file = htmlfile)
  readChar(htmlfile, file.info(htmlfile)$size)
}

# The following methods are needed to
# a) display something -> repr_text must return something otherwise repr_html is not called
# b) not return markdown/latex as otherwise repr_*.list would be called.

#' @export
repr_text.gvis <- function(obj, ...) 'gvis cannot be represented in plain text (need html)'
#' @export
repr_latex.gvis <- function(obj, ...) NULL
#' @export
repr_markdown.gvis <- function(obj, ...) NULL
