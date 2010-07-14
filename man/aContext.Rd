\name{aContext}
\alias{aContext}
\title{A container to give a different context than the default for a set of items...}
\usage{aContext(..., context, attr=list(), enabled_when, visible_when)}
\description{A container to give a different context than the default for a set of items}
\details{The basic container uses the calling model (a dialog or
item group) as its context. This allows the context to be
overridden, which might be desirable if the items are in more than
one dialog.}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Container}}}
\arguments{\item{context}{ItemGroup or item to get context from. Typically just NULL.}
\item{attr}{gWidget values passed to constructor}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
