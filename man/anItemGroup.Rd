\name{anItemGroup}
\alias{anItemGroup}
\title{Constructor for ItemGroup instances...}
\usage{anItemGroup(items=list(), name, ...)}
\description{Constructor for ItemGroup instances}
\details{An ItemGroup creates a model with properties given by the items
and a default layout for its items. This can also be specified
when the layout is drawn through \code{make\_gui}.}
\seealso{\code{\link{aContainer}} for specifying a layout}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{items}{List of Item instances or ItemGroup instances}
\item{name}{Name of ItemGroup.}
\item{...}{Passed to ItemGroup proto trait}}
\examples{\dontrun{
## make a simple item group, show in non-default layout
i <- anItemGroup(items=list(
numericItem(0,"x"),
numericItem(0,"y"),
stringItem("","z")
))
lay <- aContainer("x","y", aFrame("z", label="z in a box"))
## some proto methods:
i$make_gui(cont=gwindow("Example of itemGroup"), gui_layout=lay)
i$get_x()     # get x value
i$set_x(10)   # set x value to 10
i$to_R()      # get list of x,y,z values
}}
