\name{tableItem}
\alias{tableItem}
\title{List editor -- list <-> tree, must have special structure to list?}
\usage{tableItem(value=data.frame(V1 = "", V2 = ""), name, label=name,
    help="", tooltip="", attr=list(expand = TRUE), model, editor, ...)}
\description{List editor -- list <-> tree, must have special structure to list?
An item to display a table of data (given as a matrix or data.frame)}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Item}}}
\arguments{\item{value}{Default value of data frame}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{ignored}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200)).}
\item{model}{ignored}
\item{editor}{ignored}
\item{...}{Passed to parent proto object during call to proto}}
\examples{## to change data frame
i <- tableItem(mtcars, name="a")
i$set_a(mtcars[1:3, 1:3])}
