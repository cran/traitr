\name{aContext}
\alias{aContext}
\title{A container to give a different context that the default for a set of items...}
\usage{aContext(..., context, enabled_when, visible_when)}
\description{A container to give a different context that the default for a set of items}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{context}{ItemGroup or item to get context from}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
