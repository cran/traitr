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

#' @include itemgroup.R
roxygen()

##################################################
#' A Dialog wraps a top-level window around a collection of items which may be item groups
#' 
#' One can specify the parent when making the UI
#' Buttons are specified through the buttons property
#' @export
Dialog <- ItemGroup$proto(class=c("Dialog", ItemGroup$class),
                          ## window title
                          title="Dialog title",
                          ## called by default help button handler
                          help_string="",
                          ## !is.null to create a status bar
                          status_text=NULL,
                          ## If !is.null, specifies list to pass to gmenu
                          menu_list = NULL,
                          ## If !is.null, specifies list to pass to gmenu                          
                          toolbar_list=NULL,
                          ## Buttons are OK, Cancel, Help, or any other name. The button handler
                          ## is NAME_handler defined as follows
                          .doc_buttons=paste(
                            desc("A propoerty listing the buttons to place into dialog.",
                                 "A button with name <code>name</code> will call",
                                 "a method <code>name_handler</code>. Default handlers are defined for",
                                 "OK, Cancel, Help, Undo, Redo, but usually the OK handler would be ",
                                 "redefined for a dialog instance.",
                                 "The default set of buttons is OK, Cancel, Help.",
                                 "Use button named <code>SPACE</code> to add 12px space between",
                                 "buttons. Use button named <code>SPRING</code>to add spring between buttons (",
                                 "pushes buttons to left and right"
                                 )
                            ),
                          buttons=c("OK", "Cancel","SPACE", "Help"),#, "Undo","Redo"), 
                          ## Default button handlers. OK_handler needs to be
                          ## overridden in the instance
                          OK_handler=function(.) {
                            print(.$to_R())
                          },
                          Cancel_handler = function(.) {
                            dispose(.$get_widget('toplevel'))
                          },
                          Help_handler = function(.) {
                            gmessage(c("Help", .$help_string), icon="info", parent=.$get_widget('toplevel'))
                          },
                          Undo_handler=function(.) .$undo(),
                          Redo_handler=function(.) .$redo(),
                          ## on_realized is called after make_gui
                          on_realized = function(.) {
                            .$update_ui()
                          },
                          
                          ## This is a list, not named, of Items or ItemLists
                          ## names are found from items.
                          items=list(),  # Items or ItemList
                          widget_list=list(), # where widgets are stored
                          get_widget=function(.,key) .$widget_list[[key]],
                          ## make_gui makes the dialog (make_ui has different arguments)
                          ## make the dialog.
                          ## parent can be NULL, a gwindow instance, or a dialog instance
                          ## set visible to FALSE to suppress drawing. Use .$visible(TRUE) to show
                          .doc_make_gui=paste(
                            desc("Make gui for the dialog",
                                  "Creates top level window located near <code>parent</code>, if given.",
                                  "Creates buttons. When clicked, these call handlers of similar name. That is,",
                                  "'OK' button calls 'OK_handler'. Button name is stripped of non letters before call,",
                                  "so 'Next >>' would call 'Next_handler'.",
                                  "Call on_realized method, if defined after construction"
                                  )
                            ),
                          make_gui=function(., gui_layout=.$make_default_gui_layout(), parent=NULL, visible=TRUE) {
                            .$init_model() # initialize model ## also called in aDialog?

                            
                            if(!is.null(parent)) {
                              if(is.proto(parent) && exists("traitr", parent) && parent$is("Dialog"))
                                parent <- parent$get_widget("toplevel")
                            }
                            widgets <- list() # where widgets are to store
                            ## make window
                            widgets[['toplevel']] <- (w <- gwindow(.$title, parent=parent, visible=FALSE))

                            ## uses gWidgets -- not actionItems 
                            ## menu
                            if(!is.null(.$menu_list))
                              widgets[['menu']] <- gmenu(.$menu_list, container = w)
                            if(!is.null(.$toolbar_list))
                              widgets[['toolbar']] <- gtoolbar(.$toolbar_list, container = w)
                            
                            ## main part
                            g <- ggroup(horizontal=FALSE, cont=widgets[['toplevel']], spacing=10)


                            .$next_method("make_gui")(., container=g, gui_layout=gui_layout)



                            ## buttons
                            ## handlers called via buttonName_handler method defined
                            bg <- ggroup(cont=g)
                            sapply(.$buttons, function(i) {
                              if(i == "SPACE") {
                                addSpace(bg, 10)
                                return()
                              } else if(i == "SPRING") {
                                addSpring(bg)
                                return()
                              }
                              
                              widgets[[i]] <<- (b <- gbutton(i, cont=bg))
                              addHandlerClicked(b, function(h,...) {
                                . <- h$action$self
                                button_name <- h$action$button_name
                                ## strip off all but characters
                                button_name <- gsub("[^a-zA-Z]","",button_name)
                                .$do_call(sprintf("%s_handler", button_name), list())
                              }, action=list(self=., button_name=i))
                            })
                            ## status bar
                            if(!is.null(.$status_text))
                              widgets[['statusbar']] <- gstatusbar(.$status_text, cont=widgets[['toplevel']])

                            ## set visible if requested
                            visible(widgets[['toplevel']]) <- visible

                            ## set widgets
                            .$widget_list <- widgets
                            ## call hook for realized
                            .$do_call("on_realized")

                            .$assign_if_null("model_value_changed", function(.) .$update_ui())
#                            .$init() ## initialize controller, ... ## called in ItemGroup

                            ## we listen to ourselves
                            ## already in init_controller for 
#                            .$add_observer(.)

                            invisible()
                          },
                          ## close the gui
                          close_gui = function(.) {
                            l <- .$widget_list
                            try(dispose(l$toplevel), silent=TRUE)
                          },
                          ## toggle visibility of top level window of dialog
                          visible=function(., value=TRUE) {
                            widget <- .$get_widget('toplevel')
                            visible(widget) <- as.logical(value)
                          },
                          ## Set status bar text if one was added at construction time
                          .doc_update_status_text=paste(
                            desc("Method to update text in status bar, if one exists")
                            ),
                          update_status_text=function(., value) {
                            if(.$has_local_slot(".statusbar"))
                              svalue(.$.statusbar) <- value
                          },
                          ## return list of values from each Item and ItemGroup in items
                          ## should be able to pass to do.call for function evaluation
                          to_R=function(.) {
                            l <- list()
                            for(i in .$items) l <- c(l, i$to_R())
                            l
                          },
                          to_string=function(.) {
                            l <- list()
                            for(i in .$items) l <- c(l, i$to_string())
                            l
                          },
                          ##
                          update_ui=function(.) {
                            ## undo/redo buttons
                            undo <- .$get_widget("Undo");
                            redo <- .$get_widget("Redo");
                            if(!is.null(undo) && isExtant(undo))
                              enabled(undo) <- .$undo_can_undo()
                            if(!is.null(redo) && isExtant(redo))
                              enabled(redo) <- .$undo_can_redo()

                            
                            .$next_method("update_ui")(.)
                          } 
                          )

#' Create a Dialog instance
#'
#' @param items List of item instances to create the model for the dialog object
#' @param title Title of dialog
#' @param help_string String for default Help button
#' @param buttons Character vector of button names. "OK","Cancel","Help","Undo","Redo" are some standard ones.
#'        "SPACE" and "SPRING" adjust the layout.
#' @param ... How to pass in other properties and methods of the dialog object. For example \code{OK\_handler}.
#' @export
aDialog <- function(items=list(),
                    title="",
                    help_string="",
                    buttons=c("OK","SPACE","Cancel","Help"),
                    ...
                    ) {
  dlg <- Dialog$proto(items=items,
                      title=title,
                      help_string=help_string,
                      buttons=buttons,
                      ...)
  dlg$init_model()
  dlg
}

## #' Make GUI.
## #'
## #' Create a GUI for the items specified to the constructor. The default GUI is basic. Use a Container layout
## #' to design a GUI.
## #' @param obj A Dialog or ItemGroup instance
## #' @param gui.layout A Container instance to specify the layout. Default layout is a table
## #' @param parent For a Dialog, an optional window to center dialog on. For ItemGroup the parent container.
## #' @param visible For a dialog, do we view the dialog? If not, the visible method is provided
## #' @param ...
## #' @return Creates a GUI from the items as specified through the layout
## #' @export
## makeGUI <- function(obj, gui.layout=NULL, parent=NULL, visible=TRUE, ...) {
##   ## check if dialog, or item group
##   if(!is.proto(obj))
##     stop("Object must be proto object")
##   if(!obj$is(c("Dialog","ItemGroup")))
##     stop("Object must be instance of aDialog or anItemGroup")
##   if(obj$is("Dialog"))
##     obj$do_call("make_gui", list(gui_layout=gui.layout, parent=parent, visible=visible))
##   if(obj$is("ItemGroup")) 
##     obj$make_gui(gui_layout=gui.layout, cont=parent, ...)
##   stop(sprintf("makeGUI not defined for object of class %s",obj$class))
## }

## #' Get values from model
## #'
## #' @param obj a Dialog or ItemGroup instance
## #' @return A named list with the model values for each item
## getValues <- function(obj) {
##   ## check if dialog, or item group
##   if(!is.proto(obj))
##     stop("Object must be proto object")
##   if(!obj$is(c("Dialog","ItemGroup")))
##     stop("Object must be instance of aDialog or anItemGroup")

##   obj$to_R()
## }
