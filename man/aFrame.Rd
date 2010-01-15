\name{aFrame}
\alias{aFrame}
\title{Box container with label and visual separator to indicate grouping...}
\usage{aFrame(..., label="frame label", horizontal=FALSE, spacing=10, context,
    enabled_when, visible_when)}
\description{Box container with label and visual separator to indicate grouping}
\value{Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{label}{label for frame}
\item{horizontal}{If \code{TRUE} left to right, if \code{FALSE} top to bottom}
\item{spacing}{Space in pixels between items}
\item{context}{ItemGroup or item to get context from}
\item{enabled_when}{Method to determine when items in container should be enabled}
\item{visible_when}{Method to determine when items in container should be visible}
\item{...}{children items specified by character strings}}
