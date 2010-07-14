\name{traitR-package}
\alias{traitR-package}
\title{An interface for GUI creation using gWidgets...}
\description{An interface for GUI creation using gWidgets}
\details{This package provides an alternate interface for creating graphical user interfaces. The design was
inspired by the Traits UI module for python developed by enthought.com.
The implementation uses the MVC design pattern in the background, although the user need not be
aware of this.

For basic use, the user creates a bunch of items (the model), specifies how these will be
layed out in a simple manner (the view), specifies actions to happen (the controller) and then creates a dialog.
See \code{\link{aDialog}} for examples.

Creating basic dialogs requires no actual GUI programming
knowledge. One specifies the items by type of variable
(\code{numericItem} or \code{stringItem}, say) and the "action"
through a method call.

The package uses the \pkg{proto} package so at some level, the R
user must use that OO syntax. In particular, methods calls are
done with the notation \code{obj$method_name} and method
definitions have an initial argument \code{.} for passing in a
reference to the \code{proto} object. (The name \code{.} is a
convention, but can be changed to \code{self} or \code{this}, if
that naming convention is preferred. Methods for \code{proto}
objects are documented (to some degree anyways). Their help page
is shown through the method \code{show_help}, as in
\code{obj$show_help()}.}
\docType{package}
