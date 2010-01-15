#' An interface for GUI creation using gWidgets
#'
#' This package provides an alternate interface for creating graphical user interfaces. The design was
#' inspired by the Traits UI module for python developed by enthought.com.
#' The implementation uses the MVC design pattern in the background, although the user need not be
#' aware of this. For basic use, the user creates a bunch of items (the model), specifies how these will be
#' layed out in a simple manner (the view), specifies actions to happen (the controller) and then creates a dialog.
#' The package uses the \pkg{proto} package so at some level, the R user must use that OO syntax.
#' @name traitR-package
#' @docType package
roxygen()
