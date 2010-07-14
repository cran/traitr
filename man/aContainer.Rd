\name{aContainer}
\alias{aContainer}
\title{A container to give a different context than the default for a set of items...}
\usage{aContainer(..., context, attr=list(), enabled_when, visible_when)}
\description{A container to give a different context than the default for a set of items}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Container}}}
\arguments{\item{context}{ItemGroup or item to get context from. Typically just NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
\examples{i <- aDialog(items=list(x=numericItem(1), y=stringItem("a")))
lay <- aContainer("x","y")
\dontrun{i$make_gui(gui_layout=lay)}
## how to do enabled when
lay <- aContainer("x",
aContainer("y", enabled_when=function(.) .$get_x() > 1))
j <- i$instance()
\dontrun{j$make_gui(gui_layout=lay)}
## visible can be used to hide values if not needed
i <- aDialog(items=list(x=numericItem(1), y=stringItem("a")))
lay <- aContainer("x","y")
\dontrun{i$make_gui(gui_layout=lay)}
## how to do enabled when
lay <- aContainer("x",
aContainer("y", visible_when=function(.) .$get_x() > 1))
k <- i$instance()
\dontrun{k$make_gui(gui_layout=lay)}}
