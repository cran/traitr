\name{imageItem}
\alias{imageItem}
\title{Display an image specified by its filename.}
\usage{imageItem(value="", name, label=name, help="", tooltip="", attr=list(),
    model, editor, ...)}
\description{Display an image specified by its filename.}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Item}}}
\arguments{\item{value}{name of file}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{ignored}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200)).}
\item{model}{ignored}
\item{editor}{ignored}
\item{...}{Passed to parent proto object during call to proto}}
\examples{img <- system.file("images/plot.gif", package="gWidgets")   ## some image
i <- imageItem(img)                                         ## constructor
\dontrun{i$make_ui(container=gwindow("Image"))}                       ## show item directly}
