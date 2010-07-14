## Some test scripts for development purposes
## avoids the install, require cycle by loading with source

require(gWidgets)
options(guiToolkit="RGtk2")
require(proto)
require(digest)
require(profr)

## require(traitr) does this:
source("../R/helpers.R")
source("../R/base.R")
source("../R/model.R")
source("../R/view.R")
source("../R/controller.R")
source("../R/container.R")
source("../R/editor.R")
source("../R/items.R")
source("../R/itemgroup.R")
source("../R/dialog.R")
source("../R/itemList.R")


## Test MVC ##################################################
if(0) {
  ## test that mvc works
  model <- Model$proto(prop1=1, prop2="some value")
  view <- View$proto(make_ui=function(., cont, attr=.attr) {
                       .$widgets[['toplevel']] <- w <- gwindow()
                       g <- ggroup(cont=w, horizontal=FALSE)
                       .$widgets[["text"]] <- gtext("", cont=g)
                       .$widgets[["button"]] <- gbutton("button", cont = g)
                     })
  view$make_ui()

  ## Shows difference between defining an adapter and a more general controller
  controller <- Controller$proto(model=model,
                                 view=view,

                                 ## What it takes to make a custom handler: 2-3 methods
                                 ## update_from_model
                                 update_from_model = function(.) {
                                   .$property_prop1_value_changed <- function(., value, old_value) {
                                     svalue(.$view$widgets[['text']]) <- value
                                   }
                                 },
                                 
                                 ## transfer  back to model from view
                                 ## may not be desired. Say view component is not editable
                                 ## or model is the listener
                                 update_from_view = function(.) {
                                   widget <- .$get_view()$get_widget_by_name("text")
                                   .$append(".handlerIDs", addHandlerChanged(widget, handler = function(h,...) {
                                     . <- h$action$controller
                                     model <- .$get_model()
                                     val <- svalue(h$obj)
                                     model$set_prop1(val)
                                   }, action=list(controller=.))
                                            )
                                 },
                                 ## This is the default, could be skipped
                                 remove_view=function(.) {
                                   if(exists(".handlerIDs", .)) {
                                     sapply(".handlerIDs", function(i) removeHandler(.$get_view(), i))
                                   }
                                 }
                                 )
  ## An adapter is a special kind of controller linking a model property with one view component
  ## Here we define two, the second has no handler to call from the button back to the model
  ## rather we add a call back
  adapter <- Controller$proto(model=model, view=view,
                              ## call and adapter
                              adapters=list(
                                prop=list(
                                  property="prop1",
                                  view_widget_name="text"
                                  ),
                                ## button doesn't need to set model
                                button=list(property="prop2",
                                  view_widget_name="button",
                                  add_handler_name="")
                                )
                              
                              )

  adapter$.init()
  model$notify_observers()

  ## change model
  model2 <- Model$proto(prop1="new model", prop2="button up")
  adapter$set_model(model2)
  adapter$.init()                    # updates view from model
}

                             
                           

## testing
if(0) {
  w <- gwindow("Item test")
  i <- Item$proto(name="test", label="label", value="string", editor=Editor$proto())
  i$make_ui(cont=w)
  i$.init()
}

##################################################
## layouts

## test default lahyout
if(0) {
  i <- ItemGroup$proto(items=list(
                         numericItem(value=1, name="test", label="label"),
                         numericItem(value=1, name="test1", label="label1"),
                         numericItem(value=1, name="test2", label="label2")
                         ))
  i$make_gui(cont=gwindow("default layout"))
}

## Test the different containers
if(0) {
  i <- ItemGroup$proto(items=list(
                         numericItem(value=1, name="test", label="label"),
                         numericItem(value=1, name="test1", label="label1"),
                         numericItem(value=1, name="test2", label="label2")
                         ))

  lay <- aGroup("test")
  lay <- aFrame("test","test1", horizontal=FALSE, label="frame")
  lay <- aContainer("test","test1")
  ## Need to work on notebook -- add isn't right, pass in label?
  lay <- aNotebook(aNotebookPage("test", label="test"),
                   aNotebookPage("test1", label="testing")
                   )
#  lay <- anExpandGroup("test", "test1", label="expand")
#  lay <- aPanedGroup("test")
#  lay <- aFrame(label="test", aTableLayout("test","test1","test2", no_cols=2))
  ## no layout
#  lay <- NULL
  i$make_gui(cont=gwindow("simple test"), gui_layout=lay)
}

## test of nested containers
if(0) {
  ##A# XXX  FAILES
  i <- ItemGroup$proto(items=list(
                         string=stringItem(value="test", name="string", label="String"),
                         number=numericItem(value=NA, name="number",  label="number1"),
                         number1=numericItem(value=NA, name="number1", label="number1"),
                         number2=numericItem(value=NA, name="number2", label="number2"),
                         number3=numericItem(value=NA, name="number3", label="number3")
                         )
                       )

  cont= aFrame(aContainer(
    aFrame(aContainer("string","number"), label="test"),
    aFrame(aContainer("number1"), label="a number")
    ), label="test")

  i$make_gui(cont=gwindow("test of nested"), gui_layout=cont)

}


## test if a model for one, works for other
if(0) {
  library(gWidgets)
  options(guiToolkit="RGtk2")

  ## this item if not shown, but rather provides model for two other widget
  free <- stringItem(value="shake", name="Fair", label="free")
  
  i <- ItemGroup$proto(items=list(
                         a=stringItem(value="test", name="string", label="String"),
                         b=numericItem(value=NA, name="number", label="number"),
                         c=stringItem(name="another", model=free),
                         d=stringItem(name="another one", model=free)
                         )
                       )
  i$make_gui(cont=gwindow())
}

##################################################
## Dialogs
## basic-basic dialog
if(0) {
  i <- Dialog$proto(items=list(
                      numericItem(value=1, name="x"),
                      stringItem(value="", name="y")
                      ),
                    attr=list(),
                    status_text="fred",
                    help_string = "Finds p-value\n for a problem",
                    buttons=c("OK","Cancel", "Help"),
                    OK_handler=function(.) {
                      print(.$to_R())
                    }
                    )
  lay <- aContainer("x","y")
  i$make_gui(gui_layout=lay)
}
  

## Test of a GUI
if(0) {

  dist <- function(mean, mu, sd, alternative=c("two.sided","less","greater"),n,...) {
    alternative=match.arg(alternative)
    obs <- (mean - mu)/(sd/sqrt(n))
    switch(alternative,
           "greater"= 1 - pt(obs, df=n-1),
           "less"= pt(obs, df=n-1),
           2*(1 - pt(abs(obs), df=n-1))
           )
    }

  ## This shows how to mix and match ItemGroups (j is nested in i)
  it <-  choiceItem("two.sided", c("two.sided","less","greater"), name="alt.choice")
  ## different editors can be done as so
  it$editor$editor_name="gcheckboxgroup"
  it$editor$editor_name="gcombobox"

  sep <- separatorItem(name="sep")
  
  j <- ItemGroup$proto(items=list(
                         numericItem(value=0, name="mean", label="xbar"),
                         numericItem(value=0, name="mu", label="mu"),
                         numericItem(value=0, name="sd",  label="sd"),
                         numericItem(value=1, name="n",  label="n"),
                         it,
                         sep
                   ),
                       name="ig")

  i <- Dialog$proto(items=list(
                      j,
                      stringItem(value="two.sided", name="alt"),
                      stringItem(value="", name="output", label="p.value")
                      ),
                    attr=list(),
                    status_text="fred",
                    help_string = "Finds p-value\n for a problem",
                    buttons=c("OK","Cancel", "Help"),
                    OK_handler=function(.) {
                      output <- .$get_item_by_name("output")
                      val <- do.call(dist, .$to_R())
                      output$set_value(val)
                    }
                    )
  ## create a layout for the above
  lay <- aContainer(aFrame(aContainer("mean","sd","n","sep","alt.choice",context=j), label="statistics"),
                    aFrame(aContext("mu", context=j), "alt", label="Hypotheses", context=i),
                    aFrame("output", label="output")
                    )
  i$make_gui(gui_layout=lay)
}

## add observer to i by controller with model_value_changed


## testing
if(0) {
  w <- gwindow("Item test")
  i <- buttonItem(value="testing", action=function(.) print("hi"), name="button")
  i$set_action(function(.) print("bye"))
  i$make_ui(cont=w)
  i$.init()

}

## choice items
if(0) {

  i <- Dialog$proto(items=list(
                      
                      separatorItem(name="sep"),
                      choiceItem(value="a", values=letters[1:3], name="choice-small") ,
                      choiceItem(value="a", values=letters[1:10], name="choice-med"),
                      choiceItem(value="a", values=letters[1:26], name="choice-large", attr=list(size=c(200,200))),
                      separatorItem(name="sep"),                      
                      choiceItem(value="a", values=letters, editable=TRUE, name="choice-edit"),
                      separatorItem(name="sep"),
                      choiceItem(value="a", values=letters[1:4], multiple=TRUE, name="choice-small-mult"),
                      choiceItem(value="a", values=letters[1:26], multiple=TRUE, name="choice-large-mult")
                      ),
                       name="choices"
                       )
  i$make_gui()
}


## Things to test:

## validation with entry editor
## XXX Not working!!
if(0) {
  i <- ItemGroup$proto(items=list(x=numericItem(value=1, name="x",
                                    
                                    VALIDATION=function(., rawvalue) list(retval=sample(c(TRUE, FALSE),1), mesg="shit happens")

                                    ),
                         y = stringItem(value="", name="y")))
  i$make_ui(cont=gwindow())
}

## undo/redo support


## This example shows how to make values enabled based on other values
if(0) {
  i <- ItemGroup$proto(items=list(
                         x = numericItem(value=NA, name="x", label="x"),
                         y = numericItem(value=NA, name="y", label="y"),
                         alternative=choiceItem(value="two.sided", values=c("two.sided", "less", "greater"), name="alternative"),
                         mu = numericItem(value=0, name="mu", label="mu"),
                         paired=trueFalseItem(value=FALSE, name="paired"),
                         var.equal=trueFalseItem(value=FALSE, name="var.equal")
                         ))
  lay <- aContainer("x",
                    ## y depends on x
                    aContext("y",context=i,
                             enabled_when=function(.) {
                               ## . here is the context value, not the container object
                               val <- .$get_x()
                               !is.null(val) && !is.na(val) && (nchar(val) > 0)
                             }),
                    "alternative",
                    "mu",
                    aContainer("paired","var.equal", context=i,
                               enabled_when=function(.) {
                                 val <- .$get_y()
                                 !is.null(val) && !is.na(val) && (nchar(.$get_y()) > 0)
                               })
                    )
  i$make_gui(cont=gwindow(), gui_layout=lay)
}

## Example of variable selector -- uses a dataframeItem as a model
## data frame selecton
if(0) {
  dfi <- dataframeItem(value=".GlobalEnv", name="data.frame")
  i <- ItemGroup$proto(items=list(dfi))
  w <- gwindow("Data frame selector")
  i$make_gui(cont=w)
  
  vi <- variableSelectorItem(value="", name="varselector", dataframeItem=dfi)
  j <- ItemGroup$proto(items=list(vi))
  
  w <- gwindow("var selector")
  j$make_gui(cont=w)
}

## tooltip
if(0) {
  i <- buttonItem(value="label",
                  action=function(., h, ...) print(svalue(h$obj)),
                  tooltip="Does this owrk")
  i$make_ui(cont=gwindow("button test"))
}

## sizing of widgets somehow

## examples
## integrate in gWidgets
## wizard example (use as parent, then on_realized close 1
if(0) {
  dlg1 <- Dialog$proto(items=list(numericItem(1,name="x")),
                       title="first page",
                       buttons=c("Next","Cancel"),
                       Next_handler=function(.) {
                         print("hi")
                         dlg2$make_gui(parent=.) # parent isn't working
                       }
                       )
  dlg2 <- Dialog$proto(items=list(numericItem(1, name="y")),
                       title="Second page",
                       on_realized=function(.) dlg1$close_gui()
                       )

  dlg1$make_gui()


}
## simple with no layout
## with layout
## item group factory
## shared model
## graphics
if(0) {
  i <- Dialog$proto(items=list(
                      numericItem(10, name="n"),
                      choiceItem("rnorm", values=c("rnorm","rexp"), name="dist"),
                      graphicDeviceItem(name="graph")
                      ),
                    title="Graph",
                    help_string = "Make a graph",
                    buttons=c("OK","Cancel", "Help"),
                    OK_handler=function(.) {
                      vals <- .$to_R()
                      x <- do.call(vals$dist, list(vals$n))
                      hist(x)
                    }
                    )
  i$make_gui()
}
## adding a handler?

## a basic GUI
if(0) {
  l <- list(quit=gaction("quit", icon="quit", handler=function(h,...) dlg$close_gui()),
            help=gaction("help", icon="help", handler=function(h,...) gmessage("Call 911"))
            )

  dlg <- aDialog(items=list(
                        numericItem(10,name="x"),
                        numericItem(20, name="y")
                        ),
                      OK_handler=function(.) {
                        print(.$to_R())
                      },
                      property_x_value_changed=function(.,...) print("changed"),
                      help_string="Look ma a toolbar",
                      toolbar_list=l
                      )
  dlg$make_gui()
}


## read documentation
if(0) {
  i <- numericItem()
  i$show_help()
}

## range item and graphic
## shows how to add observer to update graphic on model value changed, rather than waiting
## until "OK" button is clicked
if(0) {
  OK_handler <- function(.) {
    n <- .$to_R()[['n']]
    hist(rnorm(n))
  }
  i <- Dialog$proto(items=list(
                         rangeItem(value=10, from=1, to = 100, by=1, name="n",
                                   attr=list(size=c(150,-1))),
                         graphicDeviceItem(name="graph")
                         ),
                    title="Example of slider and graph.",
                    help_string="Click OK to update graphic",
                    OK_handler=OK_handler
                    )
  lay <- aGroup("n","graph")

  ## add a controller to update graphic when model value changed
  i$init_model() # need to initialize models outside of make_ui to add controller
  controller <- Controller$proto(model=i, view=NULL,
                                 model_value_changed = function(.) {
                                   model <- .$get_model() # . is observer (controller instance), not model
                                   OK_handler(model)
                                 })
  i$add_observer(controller)
                                   
  
  i$make_gui(gui_layout=lay)
}

## test of button item with gaction -- pass through attr
if(0) {
  ## actions do not get widget passed back! (No h$obj available)
  a <- gaction("button name", handler=function(...) print("hi"))
  i <- buttonItem(name="button label", attr=list(action=a))
  i$make_ui(gwindow())
}

## test different validators
if(0) {
i
}

## test notebok
if(0) {
  dlg <- aDialog(items=list(a=numericItem(0), b=numericItem(0)))
  view=aNotebook(attr=list(expand=TRUE),
    aNotebookPage("a", label="one"),
    aNotebookPage("b", label="two"),
    initial_page=2
    )
 dlg$make_gui(gui_layout=view)
}

## talbe item
if(0) {
  dlg <- aDialog(items=list(
                   a=tableItem(mtcars, attr=list(size=c(300,300)))
                   ))
  dlg$make_gui()
}

if(0) {
  ## model
  dlg <- aDialog(items=list(
                   file=fileItem("", attr=list(
                                       filter=list("CSV or TXT"=list(
                                                     patterns=c("*.csv","*.txt")
                                                     ),
                                         "All files" = list(patterns=c("*"))
                                         ))),
                   header=trueFalseItem(TRUE, tooltip=paste("Variable onfirst line?")),
                   sep=stringItem("", tooltip="Field separator character"),
                   quote=stringItem("", tooltip="Set of quoting characters"),
                   dec=stringItem(".", tooltip="Character used for decimal points"),
                   as.is=trueFalseItem(!default.stringsAsFactors(), tooltip="Do not convert character to factor"),
                   na.strings=stringItem("NA", tooltip="Strings to be NA", eval=TRUE),
                   nrows=integerItem(-1, tooltip="Max number of rows to read"),
                   skip=integerItem(0, tooltip="Number of lines to skip at header"),
                   check.names=trueFalseItem(TRUE, tooltip="If TRUE ensure names are valid"),
                   fill=trueFalseItem(TRUE, tooltip="Fill unequal length rows if TRUE"),
                   strip.white=trueFalseItem(TRUE),
                   blank.lines.skip=trueFalseItem(TRUE, tooltip="If TRUE, skip blank lines"),
                   comment.char=stringItem("#", tooltip="Comment character"),
                   allowEscapes=trueFalseItem(TRUE, tooltip="C-style escapes read verbatim?"),
                   stringsAsFactors=trueFalseItem(default.stringsAsFactors(), tooltip="Characters converted to factors"),
                   fileEncoding=stringItem(""),
                   encoding=stringItem("unknown"),
                   ## our things
                   assign.to=stringItem("df", label="Assign to:"),
                   output=tableItem(attr=list(size=c(400,400)), show_label=FALSE),
                   file.type=stringItem("")
                   ),
                 title="Read in txt or csv file",
                 help_string="Select a file, then adjust paramters."
                 )
  ## view
  view <- aGroup(aNotebook(
                           aNotebookPage("file",
                                         separatorItem(),
                                         "header", "sep","quote",
                                         "dec", "fill", "comment.char",
                                         label="Main args"),
                           aNotebookPage("as.is","na.strings","nrows","skip",
                                         "check.names","fill","strip.white","blank.lines.skip",
                                         "allowEscapes","stringsAsFactors",
                                         separatorItem(),
                                         "fileEncoding", "encoding",
                                         label="Extra args")
                           ),
                 aContainer("assign.to",
                            aFrame("output", label="Preview")
                            ), 
                 horizontal=TRUE)
  ## controller 
  dlg$read_file <- function(., file.type, output, assign.to, ...) {
    if(file.type != "") {
      out <- try(do.call(sprintf("read.%s",file.type), list(...)), silent=TRUE)
      if(inherits(out, "try-error")) {
        cat("Error reading file of type,", file.type, "\n")
        out <- data.frame(V1="")
      }
    } else {
      out <- data.frame(V1="")
    }
    return(out)
  }

  dlg$model_value_changed <- function(.) {
     fname <- .$get_file()
    if(file.exists(fname)) {
      for(i in c("txt","csv")) {
        if(grepl(paste("\\.",i,sep=""), fname))
          .$set_file.type(c(txt="table",csv="csv")[i])
      }
    }
    switch(.$get_file.type(),
           "csv"={.$set_sep(","); .$set_quote('\"')},
           "table"={},
           {}
           )
    .$set_output(.$do_call("read_file",.$to_R()))
  }
  dlg$property_file_value_changed <- function(., value, old_value) {
    if(file.exists(value)) {
      for(i in c("txt","csv")) {
        if(grepl(paste("\\.",i,sep=""), value))
          .$set_file.type(c(txt="table",csv="csv")[i])
      }
    }
    switch(.$get_file.type(),
           "csv"={.$set_sep(","); .$set_quote('\"')},
           "table"={},
           {}
           )
    .$set_output(.$do_call("read_file",.$to_R()))
  }

  nms <- names(dlg$get_items())
  nms <- setdiff(nms, c("file","file.type","assign.to","output"))
  sapply(nms, function(i) {
    assign(sprintf("property_%s_value_changed",i),
           function(., ...) {
             .$set_output(.$do_call("read_file",.$to_R()))
           },
           envir=dlg)
  })
  dlg$OK_handler <- function(.) {
    out <- .$do_call("read_file",.$to_R())
    assign.to <- .$get_assign.to()
    if(exists(assign.to, envir=.GlobalEnv)) {
      if(!gconfirm(sprintf("Overwrite variable %s?", assign.to)))
        return()
    }
    assign(assign.to, out, envir=.GlobalEnv)
  }
#  dlg$add_observer(dlg)
  dlg$make_gui(gui_layout=view)
}               

## test
if(0) {
  dlg <- aDialog(items=list(x=numericItem(0)))
  dlg$make_gui()
}

## profile
if(0) {


  ## setting output (a table can be slow)
  ## WAS set a file #16 (of 120)
  ## NOW 14
  out <- profr(dlg$set_file("/Users/verzani/test.csv"))

  ## set_property -- sped up quite a bit
  ## WAS: 10=18, 100=165, 1000=1748
  ## NOW:        100=5   1000=29 -- not an issue
  l <- lapply(1:1000, function(i) numericItem(i, name=paste("X",i,sep="")))
  dlg <- aDialog(items=l)
  out <- profr(dlg$set_X1(2))

  ## to_R not an issue
  ## was      100=9, 1000=84
  l <- lapply(1:100, function(i) numericItem(i, name=paste("X",i,sep="")))
  dlg <- aDialog(items=l)
  out <- profr(dlg$to_R())
}

## test of inclusion of an itemgroup in a dialog
## makes for reuse
if(0) {
  ig <- anItemGroup(items=list(a=numericItem(0), b= stringItem("")),
                    gui_layout=aFrame("a","b", horizontal=TRUE),
#                    name="fred",
                    label="fred")
  ig$add_class("item")
  ig$make_ui <- function(., ...) ig$make_gui(...)

  dlg <- aDialog(items=list(x=numericItem(1), y = numericItem(1), fred=ig$instance()))
  view <- aContainer("x", "fred", "y")
  dlg$make_gui(gui_layout=view)
  ## to set a or b:
  dlg$get_item_by_name('fred')$set_a(1)
  ## or
  ig$set_a(2)
  ## to set x or y (in dlg proper)
  dlg$set_y(10)
}
                 

## some simple GUIS

## chooseCRANmirror
if(0) {
  m <- getCRANmirrors(all = FALSE, local.only = FALSE)[,c(1,2,4)]
  setCran <- function(.,...) {
    URL <- .$get_cran()
    print(URL)
    repos <- getOption("repos")
    repos["CRAN"] <- gsub("/$", "", URL[1L])
    options(repos = repos)
    .$close_gui()
  }
  dlg <- aDialog(items=list(
                   cran=choiceItem(value=NA, values=m,
                     show_label=FALSE,
                     attr=list(chosencol=3, size=c(400,500))
                     )
                   ),
                 title="Choose a CRAN Mirror",
                 help_string="Click a mirror, then OK, or double click on mirror",
                 OK_handler=setCran,                 # OK button click
                 property_cran_value_changed=setCran # double click
                 )
  dlg$make_gui()
}

## tkdensity
if(0) {
  replot <- function(.) {
    l <- .$to_R()
    f <- function(dist, kernel, n, bw,...) {
      if(dist=="Normal")
        y <- rnorm(n)
      else
        y <- rexp(n)
      plot(density(y,  kernel=kernel, xlim=range(y)+c(-2,2)), main="Density example")
      points(y, rep(0,n))
    }
    do.call(f,l)
  }
  dlg <- aDialog(items=list(
                   dist=choiceItem("Normal",values=c("Normal","Exponential"),
                     show_label=FALSE),
                   kernel=choiceItem("gaussian",
                     values=c("gaussian", "epanechnikov", "rectangular",
                       "triangular", "cosine"),
                     show_label=FALSE),
                   n=choiceItem(50L, as.integer(c(50,100,200,300)),
                     show_label=FALSE,
                     coerce_with=function(., value) as.numeric(value)),
                   bw=rangeItem(value=1, from=0.05, to=2.00, by=0.05,
                                   show_label=FALSE),
                   out=graphicDeviceItem()
                   ),
                 help_string="Adjust a parameter to update graphic",
                 title="tkdensity through traitr",
                 buttons="Cancel",
                 model_value_changed=replot)
  
  view <- aGroup(aContainer(aFrame("dist", label="Distribution"),
                            aFrame("kernel", label="Kernel"),
                            aFrame("n", label="Sample size"),
                            aFrame("bw", label="Bandwidth")
                            ),
                 "out",
                 horizontal=TRUE)
  dlg$make_gui(gui_layout=view)
  replot(dlg)
}


## 1. Using the choiceItem item with multiple=TRUE choices, I got an error when initializing the item with an empty selection (using character()).
if(0) {
  dlg <- aDialog(items=list(
                   a=choiceItem(character(0), values=letters[1:4], multiple=TRUE)
                   )
                 )
  dlg$make_gui()
}

## 2. The choiceItem item with multiple=TRUE choice did not work - when changing the selection in the widget, the value of the item did not change.
if(0) {
  for(n in c(3)) {
    dlg <- aDialog(items=list(
                     a=choiceItem(character(0), values=letters[1:n], multiple=TRUE, by_index=TRUE)
                     )
                   )
    dlg$make_gui()
    ## dlg$set_a(letters[c(1,3)])
    ##dlg$set_a(c(1,3))
  }
}


if(0) {
    replot <- function(.) {
    l <- .$to_R()
    m <- .$get_item_by_name("n")
    print(m$model$n)
    f <- function(dist, kernel, n, bw,...) {
      y <- switch(dist, "Normal"=rnorm(n), rexp(n))
      plot(density(y,  bw=bw, kernel=kernel), xlim=range(y)+c(-2,2), main="Density example")
      points(y, rep(0,n))
    }
    print(l)
    do.call(f,l)
  }
  dlg <- aDialog(items=list(
                 dist=choiceItem("Normal", values=c("Normal","Exponential"),
                   show_label=FALSE),
                 kernel=choiceItem("gaussian",
                   values=c("gaussian", "epanechnikov", "rectangular",
                     "triangular", "cosine"),
                   show_label=FALSE),
                   n=choiceItem(50L, as.integer(c(50,100,200,300)),show_label=FALSE),
                 bw=rangeItem(value=1, from=0.05, to=2.00, by=0.05,
                   show_label=FALSE),
                 out=graphicDeviceItem()
                 ),
               help_string="Adjust a parameter to update graphic",
               title="tkdensity through traitr",
               buttons="Cancel",
               model_value_changed=replot)
  dlg$make_gui()
}
## test itemList
if(0) {
  item <- itemList(items=list(),
                   items_name="Personnel",
                   item_factory = function(.) {
                     a <- anItemGroup(items=list(
                                        name=stringItem(""),
                                        rank=choiceItem("Private", values=c("Private","Sergeant","General")),
                                        serial.number = stringItem("", label="Serial number")))
                     a$post_process <- function(.) {
                       .$icon <- switch(.$get_rank(),
                                        "Private"="ok",
                                        "Sergeant" = "delete",
                                        "cancel")
                     }
                     a$to_string <- function(.) .$to_R()$name
                     return(a)
                   },
                   name="itemlist")
  item$make_ui(container=gwindow("Add personnel"))

}

## formula
if(0) {
  dfi <- dataframeItem(name="df")
  f <- anItemGroup(items=list(
                     df = dfi,
                     response=variableSelectorItem(dataframeItem=dfi),
                     predictor=itemList(
                       items=list(),
                       items_name="Terms",
                       item_factory=function(.) {
                         variableSelectorItem(dataframeItem=dfi)
                       }
                       )
                     ),
                   to_R = function(.) {
                     vals <- .$next_method("to_R")(.)
                     f <- paste(vals$response, "~",
                                paste(unlist(vals$predictor), collapse=" + "),
                                ",", "data=", vals$df, sep = " ")
                     f
                   })
  layout <- aGroup(aTableLayout("df","response"), aContainer("predictor"), horizontal=TRUE)
                   
  f$make_gui(container=gwindow("test"), gui_layout=layout)
}
                     
                     
                       
                                          
## tableItem isn't working
if(0) {
  i <- tableItem(mtcars)
 
  i$make_ui(container=gwindow("tableItem test"))
  i$set_value(head(mtcars))
}
