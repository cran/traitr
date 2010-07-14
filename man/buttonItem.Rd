\name{buttonItem}
\alias{buttonItem}
\title{Button item to initiate an action...}
\usage{buttonItem(value="button label", action, name, label=name, help="",
    tooltip="", attr, model, editor, ...)}
\description{Button item to initiate an action}
\details{While dialogs have a \code{buttons} property for the main buttons,
this item allows other buttons to be used within a dialog. One
must define an action (a callback) to call  when the button is
clicked. There are some issues with how this method is defined and
where it is evaluated.}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Item}}}
\arguments{\item{value}{Default value for the model}
\item{action}{function to call when clicked. Signature is
\code{function(., h, ...)  {}} (like gWidgets with extra leading
\code{.}). The "\code{.}" is the button item, not the itemgroup or
dialog that this item may be a part of. When that is the case,
\code{.$parent} refers to the parent itemgroup or dialog. The
evaluation environment is not that where the action is
defined. This can lead to unexpected sources of error.}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name. Use "" to have not label text.}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. \code{attr=list(size=c(100,200))}}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto}}
\examples{## basic button. Note the extra "." compared to gWidgets handler
b <- buttonItem("click me", action=function(.,h,...) {
print("hi")

})
## An example within a dialog
dlg <- aDialog(items=list(
a = stringItem(""),
b = buttonItem("Click me", label="", action=function(., h, ...) {
galert(sprintf("Item a is \%s\n", .$parent$get_a()))
})
),
title="A dialog with a button item",
buttons=c()               # no standard buttons
)
\dontrun{dlg$make_gui()}}
