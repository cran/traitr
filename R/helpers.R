## Some helper functions
## avoid loading in roxygen by user
if(!exists("roxygen"))
  roxygen <- function() NULL

#' Function to return value or an object (or default if value is null, NA or "")
#'
#' @param x object
#' @param default default value
#' @return Returns default if x is NA, null or "", otherwise x
get_with_default = function(x, default) {
  if(is.null(x) || is.na(x) || x == "")
    default
  else
    x
}



## doc writing helpers
## eg.
## paste(desc("A description"),param("param: a paramter"),returns("some return value"))

#' write values in paired tags with optional class
#'
#' internal function for writing proto docs
#' @param tag tag to wrap in, eg. "ul"
#' @param ... values to be wrapped. Pasted together
#' @param class optional call to add to tag
#' @return a string
wrap_in_tag <- function(tag,..., class="") {
  if(class != "")
    class=paste("class=",class,sep="")
  paste("<",tag," ", class,">",paste(...),"</",tag,">",sep="")
}
#' specify a description for documentation
#'
#' internal function for writing proto docs.
#' @param ... Values pasted in
#' return a string
desc <- function(...) wrap_in_tag("p", "Description:", ..., class="description")
#' specify a method paramter
#'
#' internal function for writing proto docs.
#' @param value name of parameter
#' @param ... Values pasted in
#' return a string
param <- function(value, ...) {
  value <- paste("<code class='param'>",value,"</code>", paste(..., sep=" ", collapse=" "))
  wrap_in_tag("p",value)
}
#' Document return value
#'
#' internal function for writing proto docs.
#' @param ... Values pasted in
#' return a string
returns <- function(...) wrap_in_tag("p", "Returns:", ..., class="returns")
#' write <ul> 
#'
#' internal function for writing proto docs.
#' @param values values to form items
#' @return returns a string
ul <- function(values) 
  wrap_in_tag("ul", paste(wrap_in_tag("li", values), collapse="\n"), "ul")

## merge
#' merge two lists, possibly overwriting
#'
#' @param x  a list
#' @param y a list. Named values of \code{y} are assigned to \code{x} and then \code{x} is returned.
#' @param overwrite If \code{TRUE} named values in \code{y} clobber similarly named values in \code{x}
#' @return Returns a list
merge.list <- function(x,y, overwrite=TRUE) {
  for(i in names(y)) {
    if(is.null(x[[i]]) || overwrite)
      x[[i]] <- y[[i]]
  }
  x
}
