\name{aTableLayout}
\alias{aTableLayout}
\title{A container for tabular layout...}
\usage{aTableLayout(..., no_cols=1, context, enabled_when, visible_when)}
\description{A container for tabular layout}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{no_cols}{Number of columns. Fills in row by row.}
\item{context}{ItemGroup or item to get context from}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
