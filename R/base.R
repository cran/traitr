#' @include helpers.R
roxygen()
## to quiet down some errors and notes in R CMD check:
require(proto)
assign(".super", NULL)

#' Base Trait to place common properties and methods
#' @export
BaseTrait <- proto(
                    ## we add a class to our objects to mimic class behavior (not dispatch though)
                    .doc_class=paste(
                      desc("There is no class information from <code>class()</code> for <code>proto</code> objects",
                           "this propertys allows us, in a lightweight manner, to keep track of a class for them.")
                      ),
                    class="TraitR",
                   ## identify as a traitr proto object
                   traitr=TRUE,
                    ## Convenience method to add to the class.
                    ## Not documented
                    add_class = function(., newclass) .$class <- c(newclass, .$class),
                    ## some methods to simplify tasks
                    ## make a new child object and call its initialization method if present
                    .doc_new=paste(
                      desc("Creates clone of proto object and calls the <code>init</code> method, if defined."),
                      param("...", "passed to <code>proto</code> call.")
                      ),
                    new = function(., ...) {
                      obj <- .$proto(...)
                      obj$do_call("init")
                      obj
                    },
                   ## assign if not null
                   .doc_assign_if_null=paste(
                     desc("Assign value or method to proto object if not present"),
                     param("key","Key to assign to"),
                     param("value", "Value to assign")
                     ),
                   assign_if_null=function(., key, value) {
                     if(!exists(key, envir=., inherits=FALSE))
                       assign(key, value, envir=.)
                   },
                    ## append to a property list
                    ## if a list, then optional key will set key for lookup
                    .doc_append=paste(
                      desc("Append a value to a property list."),
                      param("name","Property name"),
                      param("value","value"),
                      param("key","If a list, this adds a key for lookup by name. Otherwise, appended to end of list.")
                      ),
                    append=function(., name, value, key) {
                      val <- get(name, envir=.)
                      if(is.list(val)) {
                        if(!missing(key))
                          val[[key]] <- value
                        else
                          val[[length(val)+1]] <- value
                      } else {
                        val <- c(val, value)
                      }
                      assign(name, val, envir=.)
                    },

                    ## is method to check if the proto object has a class
                    ## the class is something we set, not an R class
                    .doc_is=paste(
                      desc("A function to test if object has any of classes specified."),
                      param("class","A character vector containing class names. (Not real classes, but those",
                            "defined within this package.)",
                            "Function returns <code>TRUE</code> if any mathches exists.")
                      ),
                    is = function(., class=NULL) {
                      if(!is.null(class))
                        any(class %in% .$class)
                      else
                        TRUE
                    },

                    ## return all objects in the widget
                    ## if class is non-null will return only our proto objects of that "class"
                    .doc_list_objects=paste(
                      desc("List all objects in the widget. If class is non-NULL, return only objects",
                           "matching that class (the class defined in the package)."),
                      param("class","If given, only objects of this class are returned.")
                      ),
                    list_objects = function(., class=NULL) {
                      s <- .
                      if(!s$is(class))
                        return(list())
                      out <- ls(s, all.names=TRUE)
                      while(is.proto(s <- s$parent.env())) {
                        if(s$is(class))
                          out <- c(out, ls(s, all.names=TRUE))
                      }
                      unique(out)
                    },
### Not needed?                    
##                     ## return list of objects of certain class, e.g. "Controller"
##                     .doc_list_by_class=paste(
##                       desc("Return all objects of the specified class")
##                       ),
                      
##                     list_by_class=function(., class) {
##                       out <- .$list_objects()
##                       out <- sapply(out, function(i) {
##                         obj <- get(i, envir=.)
##                         if(is.proto(obj) && obj$is(class))
##                           obj
##                       })
##                       out[!sapply(out, is.null)]
##                     },

                    ## return non functions (properties)
                    ## will return objects or just names if return_names=TRUE
                    ## can pass class= value if desired
                    .doc_list_properties=paste(
                      desc("Return all properties (non-methods) for this proto object."),
                      param("return_names","If <code>TRUE</code>returns the names of the objects",
                            "otherwise returns the objects"),
                      param("class","If non-NULL, returns only from objects of this class.")
                      ),
                    list_properties=function(., return_names=FALSE, class=NULL) {
                      out <- .$list_objects(class=class)
                      out <- sapply(out, function(i) {
                        ## skip "class" and dot names
                        if(i != "class" && !grepl("^\\.",i) && !grepl("^\\.doc_",i)) {
                          obj <- get(i, envir=.)
                          if(!is.function(obj))
                            obj
                        } else {
                          NULL
                        }
                      })
                      out <- out[!sapply(out, is.null)]
                      if(return_names)
                        names(out)
                      else
                        out
                    },
                    ## list methods
                    .doc_list_methods=paste(
                      desc("Method to list all possible methods for object")
                      ),
                    list_methods=function(.) {
                      nms <- .$list_objects()
                      ind <- sapply(nms, function(i) {
                        is.function(get(i,envir=.))
                      })
                      nms[ind]
                    },
                    
                    ## call a method if it is present. Basically do.call with
                    ## a check that the function exists
                    .doc_do_call=paste(
                      desc("Function to call method if the method exists."),
                      param("fun","method name as character"),
                      param("lst","List of arguments. Default is empty list")
                      ),
                    do_call = function(., fun, lst=list()) {
                      if(exists(fun, envir=.) && is.function(FUN <- get(fun, envir=.))) {
                        do.call(FUN, c(., lst))
                      }
                    },
                    ## doc stuff
                    ## produce a list with doc strings for each object
                    create_doc_list = function(.) {
                      ## get properties/methods with .doc_ entries
                      f <- function(s) {
                        tmp <- ls(s, all.names=TRUE)
                        ind <- grepl("^\\.doc_",tmp)
                        tmp[ind]
                      }
                      l <- list(); l$properties <- list(); l$methods <- list()
                      s <- .
                      while(is.proto(s)) {
                        for(i in f(s)) {
                          nm <- gsub("^\\.doc_","",i)
                          obj <- get(nm,envir=s)
                          if(is.function(obj) || is.null(obj))
                            type <- "methods"
                          else
                            type <- "properties"
                          if(is.null(l[[i]]))
                            l[[type]][[i]] <- list(class = s$class,
                                                   doc = get(i, envir=s))
                        }
                        s <- s$parent.env()
                      }
                      l
                    },
                    .doc_show_help = paste(
                      desc("Create HTML web page listing documented methods and properties for",
                           "the  given <code>proto</code> object"),
                      returns("Opens browser to help page.")
                      ),
                    show_help = function(.) {
                      tmpfile <- tempfile()
                      l <- .$create_doc_list()
                      cat("<html><head><style type=text/css>", file=tmpfile)
                      cat(readLines(system.file("css","traitr.css",package="traitr")),
                          sep="\n", file=tmpfile, append=TRUE)
                      ## cat("add css here", file=tmpfile, append=TRUE)
                      cat("</style></head><body>",
                          sep="\n", file=tmpfile, append=TRUE)
                      for(type in c("methods","properties")) {
                        cat("<h2>List of",type,"for object</h2>",
                            file=tmpfile, append=TRUE)
                        for(i in names(l[[type]])) {
                          if(!grepl("^\\.doc\\_Class",i)) {
                            nm <- gsub("^\\.doc\\_", "", i)
                            cat("<div class='traitrdoc'>",
                                "<h1 class='traitr'>",nm,"(",l[[type]][[i]]$class[1],")","</h1>",
                                "<div class='traitrdoc'>",l[[type]][[i]]$doc,"</div>",
                                "</div>",
                                "<hr></hr>",
                                sep="\n", file=tmpfile, append=TRUE)
                          }
                        }
                      }
                      cat("</body></html>",
                          sep="\n", file=tmpfile, append=TRUE)
                      ## browse
                      browseURL(tmpfile)
                    }
                    )

