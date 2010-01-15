\name{aPanedGroup}
\alias{aPanedGroup}
\title{A two panel paned group container.}
\usage{aPanedGroup(..., horizontal=TRUE, context, enabled_when, visible_when)}
\description{A two panel paned group container.}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{horizontal}{If \code{TRUE} left to right, if \code{FALSE} top to bottom}
\item{context}{ItemGroup or item to get context from}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
