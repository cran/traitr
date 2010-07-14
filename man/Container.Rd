\name{Container}
\alias{Container}
\title{Base Trait for Container objects.}
\description{Base Trait for Container objects. Containers are used to make views.}
\details{Basic container is a glayout object for tabular layout
There are various types of layouts. The most basic, and default view, is simply
\code{aContainer(...names of items...)} which simply uses a table to display the item's label
and editor. Other containers can be used to adjust this.

Containers have a few methods, notably the \code{is_visible} and
\code{is_enabled} methods which can be used to hide or set
sensitive to  user input the container's components.}
