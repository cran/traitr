\name{aPanedGroup}
\alias{aPanedGroup}
\title{A two panel paned group container.}
\usage{aPanedGroup(..., horizontal=TRUE, context, attr=list(), enabled_when,
    visible_when)}
\description{A two panel paned group container.}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Container}}}
\arguments{\item{horizontal}{If \code{TRUE} left to right, if \code{FALSE} top to bottom}
\item{context}{ItemGroup or item to get context from. Typically just NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
\examples{\dontrun{
i <- aDialog(items=list(x=numericItem(1), y=stringItem("a")))
lay <- aPanedGroup("x","y") ## just two children,
i$make_gui(gui_layout=lay)
## can put other children into a container to make just two children for aPanedGroup instance
j <- aDialog(items=list(x=numericItem(1), y=stringItem("a"), z=trueFalseItem(TRUE, label="check me")))
lay <- aPanedGroup("x", aContainer("y", "z"))
j$make_gui(gui_layout=lay)
}}
