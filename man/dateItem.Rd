\name{dateItem}
\alias{dateItem}
\title{A calendar date selection item...}
\usage{dateItem(value="", format_string, name, label=name, help="",
    tooltip="", attr, model, editor, ...)}
\description{A calendar date selection item}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{value}{Default data frame for the model}
\item{format\_string}{String to specify format of date to return. See \code{\link{strftime}} for codes.
default value is \code{"\%Y-\%m-\%d"}}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. \code{attr=list(size=c(100,200))}}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto}}
