\name{choiceItem}
\alias{choiceItem}
\title{Item for choosing one of several values...}
\usage{choiceItem(value="", values="", by_index=FALSE, multiple=FALSE,
    editable=FALSE, name, label=name, help="", tooltip="", attr, model,
    editor, editor_type=c("", "gradio", "gcombobox", "gtable", "gedit",
    "gcheckboxgroup"), ...)}
\description{Item for choosing one of several values}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\arguments{\item{value}{Default value for the model. This is specified by index (or indices if \code{multiple=TRUE})}
\item{values}{Values that one can select from. May be a data frame or vector. The editor depends on the size of
this: small will be radio button or checkboxes; medium is combobox; large is table}
\item{by_index}{Do we get and set the main value by index or by value?}
\item{multiple}{Multiple selection is allowed? If so, then only checkboxes or table widget is used}
\item{editable}{Can user edit value to be selected? If so, the combobox is used}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200)).
The \code{expand=TRUE} value is a default for this.}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{editor_type}{overide choice of editor by size of values. Must set attr to match desired.}
\item{...}{Passed to parent proto object during call to proto}}
\examples{## default is to get/set by value
a <- choiceItem("a", letters, name="x")
a$get_x()
a$set_x("b")
a$get_x()
## or by index, which can be easier to do
b <- choiceItem("a", letters, name="x", by_index=TRUE)
b$get_x()
b$set_x(2)
b$get_x()
## Size determines widget, unless you set editor_type
## a radio group
rg <- choiceItem("a", letters[1:3], name="x")
## a combobox
cb <- choiceItem("a", letters[1:8], name="x")
## a table
tb <- choiceItem("a", letters[1:26], name="x")
## adjust size of table widget
tb <- choiceItem("a", letters[1:26], name="x", attr=list(size=c(width=300,height=400)))
## Multiple and size determines widget type
## smaller uses checkboxgroup
cbg <- choiceItem("a", letters[1:5], multiple=TRUE)
tbl <- choiceItem("a", letters[1:15], multiple=TRUE)}
