\name{anItemGroup}
\alias{anItemGroup}
\title{Constructor for ItemGroup instances...}
\usage{anItemGroup(items=list(), name, ...)}
\description{Constructor for ItemGroup instances}
\details{An ItemGroup creates a model with properties given by the items
and a default layout for its items. This can also be specified
when the layout is drawn through \code{make_gui}.

An item group bundles a list of items into a model. When
the model is intialized, constructors to access the model values
are created. These getters/setters use the item names, so that
\code{get_name} will get the main value for the item with name
attribute "name".

An item group has the useful methods \code{to_R} to return the
values in the model as a named list and \code{get_item_by_name}
to get the item from the list of items matching the name.}
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
