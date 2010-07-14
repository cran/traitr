\name{aNotebook}
\alias{aNotebook}
\title{A notebook container.}
\usage{aNotebook(..., close_buttons=FALSE, initial_page=1, context,
    attr=list(expand = TRUE), enabled_when, visible_when)}
\description{A notebook container.}
\details{Pages of notebook are made with aNotebookPage container, which in turn can hold other items.}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Container}}}
\arguments{\item{close_buttons}{Logical indicating if close buttons should be added (RGtk2 only)}
\item{initial_page}{Which page to open on}
\item{context}{ItemGroup or item to get context from. Typically left as NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
\examples{\dontrun{
i <- aDialog(items=list(x=numericItem(1), y=stringItem("a")))
lay <- aNotebook(
aNotebookPage(label="page 1", "x"),
aNotebookPage(label="page 2", "y")
)
i$make_gui(gui_layout=lay)
}}
