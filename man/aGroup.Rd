\name{aGroup}
\alias{aGroup}
\title{A box container.}
\usage{aGroup(..., horizontal=TRUE, spacing=10, context, attr=list(),
    enabled_when, visible_when)}
\description{A box container. Packs in items left to right or top to bottom}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Container}}}
\arguments{\item{horizontal}{If \code{TRUE} left to right, if \code{FALSE} top to bottom}
\item{spacing}{Space in pixels between items}
\item{context}{ItemGroup or item to get context from. Typically just NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
\examples{i <- aDialog(items=list(xlong=numericItem(1), y=stringItem("a")))
lay <- aGroup("xlongname","y", horizontal=FALSE)  # not in nice layout
\dontrun{i$make_gui(gui_layout=lay)}}
