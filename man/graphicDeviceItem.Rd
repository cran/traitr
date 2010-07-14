\name{graphicDeviceItem}
\alias{graphicDeviceItem}
\title{A graphic device item.}
\usage{graphicDeviceItem(value="", name, label=name, help="", tooltip="",
    attr=list(size = c(480, 480)), model, editor, ...)}
\description{A graphic device item. (Only with RGtk2 and cairoDevice!)}
\details{This device will become the current one if the mouse clicks in the window, or even just crosses over the window.
This isn't perfect, but should be easy enough to get used to.
This only works with gWidgetsRGtk2}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\note{There is some thing odd that causes a display to pop up before the cairo Device if no devices are open.}
\seealso{\code{\link{Item}}}
\arguments{\item{value}{ingored}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200)).}
\item{model}{ignored}
\item{editor}{ignored}
\item{...}{Passed to parent proto object during call to proto}}
\examples{graphIt <- function(n, ...) hist(rnorm(n))
dlg <- aDialog(items=list(n=integerItem(10), out=graphicDeviceItem()),
model_value_changed=function(.) do.call("graphIt", .$to_R()) ## ... allows out to pass in unnoticed
)
\dontrun{dlg$make_gui()
graphIt(dlg$get_n()) ## initial graphic
}}
