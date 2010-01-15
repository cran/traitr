\name{choiceItem}
\alias{choiceItem}
\title{Item for choosing one of several values...}
\usage{choiceItem(value="", values="", by_index=FALSE, multiple=FALSE,
    editable=FALSE, name, label=name, help="", tooltip="", attr, model,
    editor, editor_type=c("", "gradio", "gcombobox", "gtable",
    "gedit"), ...)}
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
