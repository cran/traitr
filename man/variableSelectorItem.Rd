\name{variableSelectorItem}
\alias{variableSelectorItem}
\title{Item to select a variable (or variables) from a selected data frame...}
\usage{variableSelectorItem(value=NA, multiple=FALSE, dataframeItem, name,
    label=name, help="", tooltip="", attr, model, editor, ...)}
\description{Item to select a variable (or variables) from a selected data frame}
\details{Needs to have a dataframeItem specified to be useful.}
\seealso{\code{\link{dataframeItem}}}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Item}}}
\arguments{\item{value}{Default data frame for the model, defaults to .GlobalEnv}
\item{multiple}{Allow multiple selection?}
\item{dataframeItem}{A required dataframeItem instance. This need not be in same display, or even displayed}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200)) This widget uses a gtable instance and specifying the size is suggested}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto}}
\examples{df <- data.frame(a=1:3, b= letters[1:3], c=rnorm(3)) # make a data frame
dfI <- dataframeItem(value="df", name="dfI")
dlg <- aDialog(items=list(
dfI,                      ## a bit awkward -- can't define dfI in list of items
variable=variableSelectorItem(dataframeItem=dfI))
)
\dontrun{ dlg$make_gui() }}
