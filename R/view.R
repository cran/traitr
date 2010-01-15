#' @include model.R
roxygen()

## Use makeUI to makeUI components, updateUI to update them

##################################################
#' Trait for View objects.
#'
#' A view "displays" the values in an associated model. The association is through a controller
#' the view does not know the controller or the model
#' Views are initialized through the \code{make_ui} method.
#' @export
View <- BaseTrait$proto(class=c("View", BaseTrait$class),
                         ## properties
                         ## list of widgets in view for access by controller by name
                         ## layout
                         attr=list(),

                         ## methods
                         
                         ## get all widgets
                         widgets=list(),
                         get_widgets = function(.) .$widgets,
                         ## get widget  by name
                         get_widget_by_name= function(., key) .$get_widgets()[[key]],

                         ## makeUI sets up widget.
                         ## store widgets into .$widgets by name
                         make_ui= function(., cont, attr=.$attr) {},
                         ## is_realized is TRUE if widget is realized
                         is_realized=function(.) {
                           length(.$get_widgets()) && isExtant(.$get_widgets()[[1]])
                         },
                         ## communication between view and model done through controller
                         ## these are generic functions for a view to get and set the main value
                         ## others are specific to sub instances
                         ## These should not be called by users, they do not sychronize with model, but instead
                         ## the value refers to the view. Basic call is svalue, or svalue<-
                        get_value_from_view = function(.) {},
                        set_value_in_view = function(., widget_name, value) {
                          if(.$is_realized()) {
                            widget <- .$get_widget_by_name(widget_name)
                            blockHandler(widget)
                            svalue(widget) <- value
                            unblockHandler(widget)
                          }
                        },
                         ## methods to change visibility, enabled of the widgets
                         ## gWidgets specific
                         enabled = function(., bool) {
                           invisible(sapply(.$get_widgets(), function(i) enabled(i) <- bool))
                         },
                         visible = function(., bool) {
                           invisible(sapply(.$get_widgets(), function(i) visible(i) <- bool))
                         }
                         )

## constructor
#' Constructor for a View proto object
#'
#' Simply provides a more typical calling interface for the View proto object
#' @param ... passed to proto method for
#' @return Returns the View object. Call \code{obj$show_help()} to view its methods and properties.
#' @export
aView <- function(...) View$new(...)
