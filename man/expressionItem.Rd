\name{expressionItem}
\alias{expressionItem}
\title{Item for typing in R expressions.}
\usage{expressionItem(value="", name, label=name, help="", tooltip="", attr,
    model, editor, ...)}
\description{Item for typing in R expressions. These are evaluated in .GlobalEnv prior to return}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{numericItem}}, \code{\link{integerItem}}, \code{\link{stringItem}}, as these are
similar, but also validate the final results}
\arguments{\item{value}{Default value for the model}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200))}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto}}
