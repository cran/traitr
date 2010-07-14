\name{dialogMaker}
\alias{dialogMaker}
\title{Automatically create a dialog for a function...}
\usage{dialogMaker(f, title="Dialog", help_string="", make_gui=TRUE,
    add_graphic_device=FALSE, ...)}
\description{Automatically create a dialog for a function}
\details{Function must have a special markup for its argument. A named
argument a..b is interpreted with b determining the type of item
to use. We support numeric, string, choice, range, ???  Within the
body of the function, the variable a..b should be referred to by
a. The idea is that you write and debug the function as usual,
then simply modify the argument list to include the types. This
function will not work for functions whose arguments use lazy
evaluation referring to other argument's values.

All arguments should have a default
A choice items should have its default with a vector. The first argument is the selected one
A range item is specified with values c(from=., to=..., by=..., [value=from]). If value not give, then from is used.
The OK_handler will call f.}
\value{Returns the dialog instance}
\value{Returns an instance of \code{aDialog}.}
\arguments{\item{f}{function to make dialog for. Its arguments must be specified in a certain way.}
\item{title}{Title for dialog window}
\item{help_string}{String for help information}
\item{make_gui}{If \code{TRUE} or \code{add_graphic_device=TRUE} then call dialogs \code{make_gui} method}
\item{add_graphic_device}{If \code{TRUE} add an graphicDeviceItem to dialog}
\item{...}{passed to \code{make_gui} when no graphic device asked for}}
\examples{f <- function(x..numeric=1, y..string="a") print(list(x,y))
\dontrun{dialogMaker(f)}
## can have missing arguments
f <- function(x, y..numeric=1) print(list(x,y))
\dontrun{dialogMaker(f)}
## a choice item. Sizing is funny for tables
f <- function(x..choice=letters) print(x)
\dontrun{dialogMaker(f)}
## range items
f <- function(x..numeric=0, mu..numeric=0,
alternative..choice=c("two.sided","less","greater"),
conf.level..range=c(.80,1.00, .01, .95)) {
out <- capture.output(t.test(x, alt=alternative, conf.level=conf.level))
print(out)
}
\dontrun{dialogMaker(f, title="CI from t.test with summarized values")}}
