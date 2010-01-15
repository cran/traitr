\name{aDialog}
\alias{aDialog}
\title{Create a Dialog instance...}
\usage{aDialog(items=list(), title="", help_string="", buttons=c("OK",
    "SPACE", "Cancel", "Help"), ...)}
\description{Create a Dialog instance}
\arguments{\item{items}{List of item instances to create the model for the dialog object}
\item{title}{Title of dialog}
\item{help_string}{String for default Help button}
\item{buttons}{Character vector of button names. "OK","Cancel","Help","Undo","Redo" are some standard ones.
"SPACE" and "SPRING" adjust the layout.}
\item{...}{How to pass in other properties and methods of the dialog object. For example \code{OK\_handler}.}}
