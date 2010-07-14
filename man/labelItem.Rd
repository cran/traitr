\name{labelItem}
\alias{labelItem}
\title{Simple label item...}
\usage{labelItem(value="label", name, label, help="", tooltip="", attr, model,
    editor, ...)}
\description{Simple label item}
\details{Useful to adding text to a dialog. Has no interactivity.}
\value{A proto object. Call \code{obj$show_help()} for its methods and properties}
\arguments{\item{value}{Default value for the label}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Same as \code{value}. Here for consistency, but needn't be used}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200))}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto}}
