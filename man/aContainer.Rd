\name{aContainer}
\alias{aContainer}
\title{A container to give a different context than the default for a set of items...}
\usage{aContainer(..., context, enabled_when, visible_when)}
\description{A container to give a different context than the default for a set of items}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{context}{ItemGroup or item to get context from}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
\examples{\dontrun{
i <- anItemGroup(x=numericItem(1), y=stringItem("a"))
lay <- aContainer("x","y")
makeGUI(i, gui_layout=lay)
}}
