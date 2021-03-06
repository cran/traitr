% Generated by roxygen2 (4.0.1): do not edit by hand
\name{integerItem}
\alias{integerItem}
\title{Item for integers}
\usage{
integerItem(value = integer(0), name, label = name, help = "",
  tooltip = "", eval_first = FALSE, attr, model, editor, ...)
}
\arguments{
\item{value}{Default value for the model}

\item{name}{Required name for object. Names should be unique within a group of items}

\item{label}{Optional label, default value is the name}

\item{help}{Optional help string}

\item{tooltip}{Optional tooltip to display}

\item{eval_first}{Should value be run through eval/parse before coercion.}

\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200))}

\item{model}{Optional model. Useful if one wishes to use same model for multiple views}

\item{editor}{Specification of editor (a view) to override default}

\item{...}{Passed to parent proto object during call to proto}
}
\value{
A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
}
\description{
Item for integers
}
\seealso{
\code{\link{numericItem}}
}

