##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

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
