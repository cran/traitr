\name{aTableLayout}
\alias{aTableLayout}
\title{A container for tabular layout...}
\usage{aTableLayout(..., no_cols=1, context, attr=list(), enabled_when,
    visible_when)}
\description{A container for tabular layout}
\details{The basic container has one column for the item's labels and one
column for the item's editors.}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{aContainer}} constructor, \code{\link{Container}} base trait}
\arguments{\item{no_cols}{Number of columns. Fills in row by row.}
\item{context}{ItemGroup or item to get context from. Typically just NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
\examples{## simple example
i <- aDialog(items=list(x=numericItem(1), y=stringItem("a")))
lay <- aTableLayout("x","y", no_cols=2)
\dontrun{i$make_gui(gui_layout=lay)}}
