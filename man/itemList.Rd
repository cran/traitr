\name{itemList}
\alias{itemList}
\title{An itemList is used to store a list of similar items or itemgroups with a means to edit individually...}
\usage{itemList(items=list(), items_name="", item_factory, name, label=name,
    help="", tooltip="", attr=list(), model, editor, ...)}
\description{An itemList is used to store a list of similar items or itemgroups with a means to edit individually}
\note{This item's model is a a list storing child items or item groups.
To create new items, the \code{item_factory} method should be provided. It provides a
template for a new item, the editor allows the user to modify its values
When a child item is edited the "done" button is clicked to close. The method \code{post_process}
is called. (The edited changes may already have been sent back to the model.)
The child items \code{to_string} method is called to make the label in the table that allows
the user to select the child item to edit. This should be a character vector of length 1.
The table can display an icon. Simply set the \code{icon} property of the icon to a \pkg{gWidgets}
stock icon name.

The child items can be returned via the \code{get_value} method or the \code{get_NAME} method, where
\code{NAME} is that passed into the \code{name} argument of the constructor.
The \code{to_R} method can be modified to manipulate the return value. The vignette has an example
where the output is coerced into a data frame. The default is a list with each child items \code{to_R}
method called to form the numbered components.}
\value{A \code{proto} object. Call \code{obj\$show_help()} to view its methods and properties.}
\arguments{\item{items}{list of similar items, may be empty list}
\item{items_name}{Header name on top of table displaying item list}
\item{item_factory}{function to call to produce a new item, e.g. \code{function(.) numericItem(1)}}
\item{name}{name of itemList object}
\item{label}{label for itemList object}
\item{help}{help string}
\item{tooltip}{tooltip}
\item{attr}{attributes passed to \code{make_ui} constructor}
\item{model}{optional model to pass in}
\item{editor}{optional editor to pass in}
\item{...}{passed along to \code{Item\$proto()} call}}
\examples{\dontrun{
## make icons
imagedir <- system.file("images",package="traitr")
addStockIcons(gsub("\\\\.png","", list.files(path=imagedir)),
list.files(path=imagedir, full.names=TRUE))
## make item
item <- itemList(items=list(),
items_name="Personnel",
item_factory = function(.) {
a <- anItemGroup(items=list(
name=stringItem(""),
rank=choiceItem("Scout", values=c("Scout","Captain","General")),
serial.number = stringItem("", label="Serial number")))
a$post_process <- function(.) {
.$icon <- tolower(.$get_rank())
}
a$to_string <- function(., drop=TRUE) .$to_R()$name
return(a)
},
name="itemlist")

item$make_ui(container=gwindow("itemList test"))
}}
