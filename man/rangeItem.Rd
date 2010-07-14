\name{rangeItem}
\alias{rangeItem}
\title{A range selection item...}
\usage{rangeItem(value="", from=0, to=10, by=1, name, label=name, help="",
    tooltip="", attr, model, editor, ...)}
\description{A range selection item}
\details{Editor is a slider (with spinbutton when \code{by} value is an integer).}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{value}{Default data frame for the model}
\item{from}{Starting value of range}
\item{to}{Ending value of range}
\item{by}{Step size to step through range. If an integer, a spinbutton is also displaye}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200))}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to Item trait}}
\examples{i <- rangeItem(value=5, from=0, to=10, by=1, name="rng")
i$get_rng()
i$set_rng(10)
i$get_rng()}
