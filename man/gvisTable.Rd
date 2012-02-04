\name{gvisTable}
\alias{gvisTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Google Table Chart with R
}
\description{
  The gvisTable function reads a data.frame and
  creates text output referring to the Google Visualisation API, which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser.
  
  A table that can be sorted and paged. Table cells can be formatted
  using format strings, or by directly inserting HTML as cell
  values. Numeric values are right-aligned; boolean values are displayed
  as check marks. Users can select single rows either with the keyboard
  or the mouse. Users can sort rows by clicking on column headers. The
  header row remains fixed as the user scrolls. The table fires a number
  of events corresponding to user interaction.
}
\usage{
gvisTable(data, options = list(), chartid)
}
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a table
  }
  \item{options}{list of configuration options for Google Table.
         \describe{
\item{\code{gvis.editor}}{a character label for an
 on-page button which opens an in-page dialog box that enables
users to edit, change and customise the chart. By default no
value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/table.html#Configuration_Options}:
  
    \describe{
      \item{\code{allowHtml}}{boolean. Default \code{FALSE}.
	If set to \code{TRUE}, formatted values of cells that include HTML
	tags will be rendered as HTML. If set to \code{FALSE},
	most custom formatters will not work properly.
      }
      \item{\code{alternatingRowStyle}}{boolean. Default \code{TRUE}
	Determines if alternating color style will be assigned to odd and
	even rows.
      }
      \item{\code{cssClassNames}}{
	An object in which each property name describes a table element, and the
	property value is a string, defining a class to assign to that table
	element. Use this property to assign custom CSS to specific elements of
	your table. To use this property, assign an object, where the property
	name specifies the table element, and the property value is a string,
	specifying a class name to assign to that element. You must then define
	a CSS style for that class on your page. The following property names
	are supported:
	
	\code{headerRow} - Assigns a class name to the table header row (<tr> element).
	
	\code{tableRow} - Assigns a class name to the non-header rows (<tr> elements).
	
	\code{oddTableRow} - Assigns a class name to odd table rows (<tr>
	elements). Note: the \code{alternatingRowStyle} option must be set to true.
	
	\code{selectedTableRow} - Assigns a class name to the selected table row (<tr>
	element).
	
	\code{hoverTableRow} - Assigns a class name to the hovered table row (<tr>
	element).
	
	\code{headerCell} - Assigns a class name to all cells in the header row (<td>
	element).
	
	\code{tableCell} - Assigns a class name to all non-header table cells (<td> element).

	\code{rowNumberCell} - Assigns a class name to the cells in the row number
	column (<td> element). Note: the \code{showRowNumber} option must be set to
	\code{TRUE}.
	
	Example: var cssClassNames = \{headerRow: 'bigAndBoldClass',
	hoverTableRow: 'highlightClass'\};
      }
      \item{\code{firstRowNumber}}{number. Default 1.
	The row number for the first row in the data. Used only if
	\code{showRowNumber} is \code{TRUE}. 
      }
      \item{\code{height}}{string. Sets the height of the visualization's
	container element. You can use standard HTML units (for example,
	'100px', '80em', '60'). If no units are specified the number is assumed
	to be pixels. If not specified, the browser will set the width
	automatically to fit the table; if set smaller than the size required by
	the table, will add a vertical scroll bar.
      }
      \item{\code{page}}{string. Default \code{'disable'}.
	If and how to enable paging through the data. Choose one of
	the following string values: 
	
	\code{'enable'} - The table will include page-forward and page-back
	buttons. Clicking on these buttons will perform the paging operation and
	change the displayed page. You might want to also set the \code{pageSize}
	option.
	
	\code{'event'} - The table will include page-forward and page-back buttons, but
	clicking them will trigger a 'page' event and will not change the
	displayed page. This option should be used when the code implements its
	own page turning logic. See the TableQueryWrapper example for an example
	of how to handle paging events manually.
	
	\code{'disable'} - [Default] Paging is not supported.
      }
      \item{\code{pageSize}}{number. Default	10. The number of
	rows in each page, when paging is enabled with the page option.
      }
      \item{\code{rtlTable}}{boolean. Default \code{FALSE}.	Adds basic
	support for right-to-left languages (such as Arabic or Hebrew) by
	reversing the column order of the table, so that column zero is the
	rightmost column, and the last column is the leftmost
	column. This does not affect the column index in the underlying
	data, only the order of display. Full bi-directional (BiDi)
	language display is not supported by the table visualisation
	even with this option. This option will be ignored if you enable paging
	(using the page option), or if the table has scroll bars because you
	have specified height and width options smaller than the required table	
	size.
      }
      \item{\code{scrollLeftStartPosition}}{number. Default	0. Sets the
	horizontal scrolling position, in pixels, if the table has
	horizontal scroll bars because you have set the width
	property. The table will open scrolled that many pixels past the
	leftmost column.
      }
      \item{\code{showRowNumber}}{boolean. Default \code{FALSE}. If set
	to true, shows the row number as the first column of the table.
      }
      \item{\code{sort}}{string. Default \code{'enable'}.
	If and how to sort columns when the user clicks a column heading. If
	sorting is enabled, consider setting the \code{sortAscending}
	and \code{sortColumn} properties as well. Choose one of the
	following string values:
		
	\code{'enable'} - [Default] Users can click on column headers to sort by the
	clicked column. When users click on the column header, the rows will be
	automatically sorted, and a 'sort' event will be triggered.
	
	\code{'event'} - When users click on the column header, a 'sort' event will be
	triggered, but the rows will not be automatically sorted. This option
	should be used when the page implements its own sort. See the
	TableQueryWrapper example for an example of how to handle sorting events
	manually.
	
	\code{'disable'} - Clicking a column header has no effect.
      }
      
      \item{\code{sortAscending}}{boolean. Default \code{TRUE}. The order in
	which the initial sort column is sorted. True for ascending,
	false for descending. Ignored if 
	\code{sortColumn} is not specified.
      }
      \item{\code{sortColumn}}{number. Default -1.	An index of a column in
	the data table, by which the table is initially sorted. The column
	will be marked with a small arrow indicating the sort order.
      }
      \item{\code{startPage}}{number. Default 0. The first table page to
	display. Used only if page is in mode enable/event.
      }
      \item{\code{width}}{string. Sets the width of the visualisation's
	container element. You can use standard HTML units (for example, 
	'100px', '80em', '60'). If no units are specified the number is assumed 
	to be pixels. If not specified, the browser will set the width
	automatically to fit the table; if set smaller than the size required by
	the table, will add a horizontal scroll bar.
      }
      %%   \item{\code{gvis.listener.jscode}}{ character string which will be
      %%	placed inside select event. A valid value is
      %%	\code{alert('a table row was selected');}.
      %%	You may also use the method \code{getSelection}.
      %%     }             
    }
  }
  \item{chartid}{character. If missing (default) a random chart id will be generated based on
    chart type and \code{\link{tempfile}}
  }
}

%%\details{
  %%  ~~ If necessary, more details than the description above ~~
%%}
\value{

 \code{gvisTable} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'Table'}
  \item{\code{chartid}}{character id of the chart object. Unique chart
    ids are required to place several charts on the same page.
  }
  \item{\code{html}}{a list with the building blocks for a page
    \describe{
      \item{\code{header}}{a character string of a html page header:
	\code{<html>...<body>},}
      \item{\code{chart}}{a named character vector of the chart's building blocks:
      	\describe{
	  \item{\code{jsHeader}}{Opening \code{<script>} tag and
	    reference to Google's JavaScript library.
	  }
	  \item{\code{jsData}}{JavaScript function defining the input
	    \code{data} as a JSON object.
	  } 
	  \item{\code{jsDrawChart}}{JavaScript function
	    combing the data with the visualisation API and user
	    options.
	  }
	  \item{\code{jsDisplayChart}}{JavaScript function calling the
	    handler to display the chart.
	  }
	  \item{\code{jsChart}}{Call of the \code{jsDisplayChart} 
	    function.
	  }
	  \item{\code{jsFooter}}{End tag \code{</script>}.
	  }
	  \item{\code{divChart}}{\code{<div>} container to embed the chart
	    into the page.
	  }
	}   
      }
      \item{\code{caption}}{character string of a standard caption,
	including data name and chart id.
      }	
      \item{\code{footer}}{character string of a html page footer:
	\code{</body>...</html>}, including the used R and googleVis version
	and link to Google's Terms of Use.}
    }
  }
}

\references{
  Google Table  API: \url{http://code.google.com/apis/visualization/documentation/gallery/table.html}
  
  Follow the link for Google's data policy.
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and plotting methods. 
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Flash and Internet
## connection to display the visualisation.

## Table with links to wikipedia (flags) 
tbl1 <- gvisTable(Population)
plot(tbl1)

## Table with enabled paging
tbl2 <- gvisTable(Population, options=list(page='enable', height=300))

plot(tbl2)
}

\keyword{ iplot }
