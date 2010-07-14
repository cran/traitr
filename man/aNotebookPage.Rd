\name{aNotebookPage}
\alias{aNotebookPage}
\title{A page in a notebook...}
\usage{aNotebookPage(..., label, context, attr=list(), enabled_when,
    visible_when)}
\description{A page in a notebook}
\details{Container to hold a page within a notebook container}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{label}{Tab label for notebook page}
\item{context}{ItemGroup or item to get context from. Typically just NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
