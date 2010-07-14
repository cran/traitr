\name{aDialog}
\alias{aDialog}
\title{Create a Dialog instance...}
\usage{aDialog(items=list(), title="", help_string="", buttons=c("OK",
    "SPACE", "Cancel", "Help"), ...)}
\description{Create a Dialog instance}
\details{A dialog is like an item group, in that it combines items
into a model. However, an item group is meant to be incorporated
into other GUIS, whereas a dialog creates its own window and
decorations. A dialog has default buttons, and options for adding
in menubars, toolbars, and statusbars. The choice of buttons can
be specified at construction.
\cr

Methods:

The main method that a dialog has is
its \code{OK_handler} which is a method called with the "OK"
button is clicked (one of the default buttons). 

The getters and setters for the main value for an item are
\code{get_NAME} and \code{set_NAME}, where \code{NAME} is the
property name. The name is specified when the item is constructed
(through its \code{name} property) or more conveniently, taken
from the name of the component in the \code{items} list that
defines the items for  dialog or item group.

The method \code{to_R} returns the items values as a list (useful in combination with \code{do.call}).

The method \code{get_item_by_name} returns an item object by its
name. (Names should be unique.) This is useful if more properties
than the main one for an item are needed to be set. (The main
value is set via the setter.) The example shows how the validate
property of some items can be set.

The method \code{is_valid} is \code{TRUE} if all items validate and \code{FALSE} otherwise.

The method \code{model_value_changed(.)} is called whenever an
item property is changed either through the GUI. A dialog observes
itself.

For each item one can listen for changes with the method \code{property_NAME_value_changed(., value, old_value)}.

Properties that are of interest:
\enumerate{

\item{\code{status_text}}{ If non-NULL, when GUI is drawn, a status bar will be made with this text. The method \code{set_status_text} can be used to update the status}

\item{\code{menu_list}}{ A menu list to specify a menubar. (See \code{\link{gmenu}}.)}

\item{\code{toolbar_list}}{ A menu list to specify a toolbar. (See \code{\link{gtoolbar}}.)}

\item{\code{buttons}}{ A list of buttons names. The default is
\code{c("OK", "SPACE", "Cancel", "Help")}. The special names
\code{SPACE} and \code{SPRING} adjust their positioning, otherwise
the values are button names. When a button is clicked, the handler
\code{buttonname_handler} is called, where the buttonname is
stripped on non-alphanumeric characters. The basic buttons and
\code{Redo} and \code{Undo} have default handlers. Likely, only
\code{OK_handler} will need redefining. The property
\code{default_button} can be specified to make a button the
default one (so that it is activated when a user presses the enter
key).}
}}
\value{Returns a proto object. See its \code{show_help} method for details.}
\arguments{\item{items}{List of item instances to create the model for the
dialog object. May also be an item group
(\code{\link{anItemGroup}}).}
\item{title}{Title of dialog}
\item{help_string}{String for default Help button}
\item{buttons}{Character vector of button names. "OK","Cancel","Help","Undo","Redo" are some standard ones.
"SPACE" and "SPRING" adjust the layout.}
\item{...}{How to pass in other properties and methods of the dialog object. For example \code{OK_handler}.}}
\examples{## a Basic example
dlg <- aDialog(items=list(
a = numericItem(0),
b = stringItem("a")
),
title="The title",
help_string="Help on this dialog"
)
\dontrun{dlg$make_gui()}
## example with model_value_changed
plotIt <- function(n, mean, sd, ...) hist(rnorm(n, mean, sd))
dlg <- aDialog(items=list(
n = integerItem(10),
mean = numericItem(0),
sd = numericItem(1),
out=graphicDeviceItem()
),
buttons="Cancel",
model_value_changed=function(.) if(.$is_valid()) do.call("plotIt", .$to_R())
)
## validation for n, sd
n <- dlg$get_item_by_name("n")
n$validate <- function(., rawvalue) if(rawvalue <= 1) stop("n must be positive integer") else rawvalue
sd <- dlg$get_item_by_name("sd")
sd$validate <- function(., rawvalue) if(rawvalue <- 0) stop("sd must be positive") else rawvalue
\dontrun{dlg$make_gui()}
## subtle point about scope. Proto methods can be defined via $<- or [[<- but there is a difference.
## $<- does not have lexical scope whereas [[<- does. The $<- might be more natural to type,
## but [[<- might be more natural to use. In this example, The "b" button does not work, as it can't find the
## function a -- the frame of evaluation is the environment dlg (not its enclosing frame).
## Thanks to Gabor for his help with this.
scope_example <- function() {
a <- function(...) print("hi")
dlg <- aDialog(items=list(),
buttons=c("a","b","c"),
a_handler=function(.) a(),   ## like [[<-, not $<-
title="a, c work; b doesn't"
)
dlg$b_handler <- function(.) a()  ## $<- has unbound variables found in dlg
dlg[['c_handler']] <- a           ## [[<- uses lexical scope for unbound variables
}
\dontrun{scope_example()}}
