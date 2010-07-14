\name{anExpandGroup}
\alias{anExpandGroup}
\title{Expanding group.}
\usage{anExpandGroup(..., label="", horizontal=FALSE, expanded=TRUE, context,
    attr=list(), enabled_when, visible_when)}
\description{Expanding group. Has trigger to show/hide its children}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Container}}}
\arguments{\item{label}{label for trigger}
\item{horizontal}{If \code{TRUE} left to right, if \code{FALSE} top to bottom}
\item{expanded}{Initial state of children. Set to \code{TRUE} to show}
\item{context}{ItemGroup or item to get context from. Typically just NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
\examples{i <- aDialog(items=list(x=numericItem(1), y=stringItem("a")))
lay <- anExpandGroup(label="label frame",
aContainer("x","y"))
\dontrun{i$make_gui(gui_layout=lay)}}
