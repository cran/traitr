\name{numericItem}
\alias{numericItem}
\title{Item for numbers...}
\usage{numericItem(value=numeric(0), name, label=name, help="", tooltip="",
    eval_first=FALSE, attr, model, editor, ...)}
\description{Item for numbers}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Item}}}
\arguments{\item{value}{Default value for the model}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{eval_first}{Should value be run through eval/parse before coercion.}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200))}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto}}
\examples{## basic use
a <- numericItem(0, name="x")
a$set_x(10)
a$get_x()
## eval can be instructed
a <- numericItem(0, name="x", eval_first=TRUE)
a$set_x("1:5")
a$get_x()
a$to_R()}
