\name{dataframeItem}
\alias{dataframeItem}
\title{Item to select a data frame from the available data frames in...}
\usage{dataframeItem(value="", name, label=name, help="", tooltip="", attr,
    model, editor, ...)}
\description{Item to select a data frame from the available data frames in .GlobalEnv}
\details{This widget checks every second or so for new data frames and updates selection accordingly}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{value}{Default data frame for the model, defaults to .GlobalEnv}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200)) This widget uses a gtable instance and specifying the size is suggested}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto}}
