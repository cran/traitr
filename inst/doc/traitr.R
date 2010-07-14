###################################################
### chunk number 1: 
###################################################
## function to use booktabs format for tables
## colTypes -- l r c p ... for latex

## eg. cat(booktabs(df, colTypes=c("l","l","p{.65\\textwidth}")))
##     cat(booktabs(df, caption="Plot command"))
booktabs <- function(x,caption="",subtop="", label="",
                     colTypes=NULL, ...) UseMethod("booktabs")

booktabs.matrix <- function(x,caption="",subtop="",label="",colTypes=NULL,...) {
  dnms <- dimnames(x)
  hasDimnames <- ifelse(!is.null(names(dnms)), TRUE, FALSE)

  hasRowNames <- TRUE
  numRowNames <- try(as.numeric(dnms[[1]]), silent=TRUE)
  if(!inherits(numRowNames, "try-error") &&
     all( numRowNames == 1:nrow(x))) {
    ## skip numbers
    hasRowNames <- FALSE
  }

  ## make character matrix using format
  m <- matrix(rep("", (nrow(x) + hasRowNames) * ncol(x)), nrow= nrow(x))

  ## colTypes -- vector of 
  if(is.null(colTypes)) {
    colTypes <- character(ncol(x))
    for(i in 1:ncol(x)) {
      colTypes[i] <- switch(class(x[,i])[1],
                            "numeric" = "r",
                            "logical" = "c",
                            "l")
    }
  }
  
  ## fill
  if(hasRowNames) {
    if(hasDimnames) {
      m[,1] <- paste("\\quad ", dnms[[1]], sep="")
    } else {
      m[,1] <- dnms[[1]]
    }
  }
  
  ## fill in column by column
  for(j in 1:ncol(x))
    m[, j + hasRowNames] <- switch(class(x[,j])[1],
                                   "numeric"=format(x[,j], digits=4),
                                   as.character(x[,j]))

  ## write out
  out <- paste("\\begin{table}",
               "\\centering",
               if(label != "") {
                 paste("\\label{", label, "}", sep="")
               },
               if(caption != "") {
                 paste("\\caption{", caption, "}", sep="")
               },
#               if(subtop != "") {
#                 paste("\\subtop{", subtop, "}", sep="")
#               },
               paste("\\begin{tabular}{",
                     "@{}",rep("l@{\\quad}",hasRowNames),
                     paste(colTypes,collapse=""),
                     "@{}}",
                     sep=""),
               "\\toprule",
               if(!is.null(names(dnms)[2])) 
                 paste("\\multicolumn{", ncol(x) + hasRowNames, "}{c}{", names(dnms)[2], "}\\\\", sep=""),
               ## header row
               if(hasRowNames)  {
                 paste(paste(c(names(dnms)[1], dnms[[2]]), collapse="&"), "\\\\", sep="")
               } else {
                 paste(paste(dnms[[2]], collapse="&"), "\\\\", sep="")
               },
               "\\midrule",
               paste(apply(m, 1, function(i)
                           paste(i, collapse="&")), collapse="\\\\"),
               "\\\\ \\bottomrule",
               "\\end{tabular}",
               "\\end{table}",
               "",
               sep="\n")

  out
}
booktabs.data.frame <- booktabs.matrix
  

booktabs.table <- function(x, caption="",subtop="",label="",
                           colTypes=NULL, ...) {
  y <- x[,]
  dimnames(y) <- dimnames(x)
  booktabs(y, caption, subtop, label, ...)
}


###################################################
### chunk number 2: 
###################################################
options(prompt=" ")
options(continue=" ")
options(width=80)


###################################################
### chunk number 3: 
###################################################
require(traitr)
require(gWidgets)
options(guiToolkit="RGtk2")


###################################################
### chunk number 4: 
###################################################
dlg <- aDialog(items=list(
                 number=numericItem(0),
                 string=stringItem("")
                 )
               )
dlg$make_gui()                          # method call using $ notation.


###################################################
### chunk number 5: 
###################################################
dlg <- aDialog(items=list(
                 number=numericItem(0),
                 string=stringItem("")
                 ),
               OK_handler=function(.) { # . is reference to dlg object
                 values <- .$to_R()
                 f <- function(number,string) 
                   cat("number is",number, "string is",string,"\n")
                 do.call(f, values)
               }
               )
dlg$make_gui()


###################################################
### chunk number 6: 
###################################################
basic.t.test <- function(mean, mu, sd, alternative=c("two.sided","less","greater"),
                         n, ...) {
  alternative <- match.arg(alternative)
  obs <- (mean - mu)/(sd/sqrt(n))
  switch(alternative,
         "greater" = 1 - pt(obs, df=n-1),
         "less" = pt(obs, df=n-1),
         2*(1 - pt(abs(obs), df=n-1))
         )
}


###################################################
### chunk number 7: 
###################################################
dlg <- aDialog(items=list(
                 mean = numericItem(0),
                 sd = numericItem(1),
                 n = numericItem(5),
                 mu = numericItem(0),
                 alternative=choiceItem("two.sided", values=c("two.sided","less","greater")),
                 output = stringItem("", label="p.value")
                 ),
               title="A basic t-test interface",
               help_string="Enter in summarized values then click OK to get the p-value",
               OK_handler=function(.) {
                 lst <- .$to_R()
                 lst$output <- NULL     # not really needed, ignored by function 
                 out <- do.call("basic.t.test", lst)
                 .$set_output(out)
               }
               )
dlg$make_gui()


###################################################
### chunk number 8: 
###################################################
view <- aContainer("mean","sd","n","mu","alternative","output")


###################################################
### chunk number 9: 
###################################################
view <- aContainer(aFrame("mean","sd","n", label="Statistics"),
                   aFrame("mu", "alternative", label="Hypotheses"),
                   "output")


###################################################
### chunk number 10: 
###################################################
view <- aContainer(aFrame(aContainer("mean","sd","n"), label="Statistics"),
                   aFrame(aContainer("mu","alternative"),label="Hypotheses"),
                   separatorItem(),
                   "output")


###################################################
### chunk number 11: 
###################################################
dlg1 <- dlg$instance() ## instead of copying the definition above.
dlg1$make_gui(gui_layout=view)


###################################################
### chunk number 12: 
###################################################
positive_value <- function(., rawvalue) {
  value <- as.numeric(rawvalue)
  if(!value > 0)
    stop("value is not positive")
  value
}


###################################################
### chunk number 13: 
###################################################
sd <- numericItem(1, name="sd")
sd$validate <- positive_value


###################################################
### chunk number 14: 
###################################################
sd <- dlg$get_item_by_name("sd")         # lookup and return item by name
sd$validate <- positive_value            # assigns method to item
dlg1 <- dlg$instance()
dlg1$make_gui(gui_layout=view)


###################################################
### chunk number 15: 
###################################################
dlg <- aDialog(items=list(
                 x = numericItem(NA, eval=TRUE),
                 y = numericItem(NA, eval=TRUE),
                 alternative=choiceItem("two.sided", 
                   values=c("two.sided", "less", "greater")),
                 mu = numericItem(0),
                 paired=trueFalseItem(FALSE),
                 var.equal=trueFalseItem(FALSE)
                 ),
               title="GUI with some parts disabled"
               )
view <- aContainer("x",
                   aContext("y", context=dlg,
                            enabled_when=function(.) { # y depends on x
                              ## . here is the context value, not the container object
                              val <- .$to_R()$x
                              !is.null(val) && !is.na(val)  && (nchar(val) > 0)
                            }),
                   "alternative",
                   "mu",
                   aContainer("paired","var.equal", context=dlg,
                              enabled_when=function(.) {
                                val <- .$to_R()$y
                                !is.null(val) && !is.na(val) &&  (nchar(.$get_y()) > 0)
                              })
                   )
dlg$make_gui(gui_layout=view)


###################################################
### chunk number 16: 
###################################################
drawGraph <- function(n,..) hist(rnorm(n))
dlg <- aDialog(items=list(
                 n=rangeItem(10, from=1, to=100, by=1),
                 graph=graphicDeviceItem()
                 ),
               title="Draw a graph",
               help_string="Adjust slider or click OK to produce a new graph",
               model_value_changed=function(.) {
                 l <- .$to_R()
                 l$graph <- NULL        # not really necessary
                 do.call("drawGraph", l)
               },
               OK_handler=function(.) do.call("drawGraph",.$to_R())
               )
               
dlg$make_gui()


###################################################
### chunk number 17: 
###################################################
df <- rbind(
            c("stringItem","For holding strings"),
            c("numericItem","For numbers"),
            c("integerItem","For integers"),
            c("expressionItem","For R expressions"),
            c("trueFalseItem","For Boolean values"),
            c("choiceItem","For choosing one or more values from a list of possible values"),
            c("rangeItem","To select a value from a range of values"),
            c("buttonItem","For adding a button"),
            c("labelItem","For adding a label"),
            c("dateItem","For editing a calendar date."),
            c("separatorItem","To add a visual separator"),
            c("dataframeItem","To select a data frame"),
            c("variableSelectorItem","To select a variable from a data frame"),
            c("graphicDeviceItem","(RGtk2 only) To embed a graphic device"),
            c("formulaItem", "For formula specification (to be written)"),
            c("dfeditItem", "To edit a data set (to be written)"),
            c("itemList", "An item that stores a list of other items (or itemgroups)")
            )
colnames(df) <- c("Constructor","Description")
cat(booktabs(df,
             colTypes=c("l","p{0.7\\textwidth}"),
             caption="Table of item constructors.",
             label="tab:item-constructors"))


###################################################
### chunk number 18: 
###################################################
dfi <- dataframeItem(value=".GlobalEnv", name="dfi", 
                     editor_style="compact") # alternative editor style
dlg <- aDialog(items=list(
                 dfi,
                 vsi=variableSelectorItem("", multiple=FALSE, dataframeItem=dfi, 
                   attr=list(size=c(200,200)))
                 ))
dlg$make_gui()


###################################################
### chunk number 19: 
###################################################
hyps <- anItemGroup(items=list(
                      mu=numericItem(0), 
                      alternative=choiceItem("two.sided", c("two.sided","less","greater"))
                      ),
                    gui_layout=aFrame("mu","alternative", label="Hypotheses")
                    )
ttestDialog <- aDialog(items=list(
                         x=numericItem(NA, eval=TRUE),
                         y=numericItem(NA, eval=TRUE),
                         hyps$instance()
                         ),
                       OK_handler=function(.) {
                         do.call("t.test",.$to_R())
                       }
                       )
wilcoxDialog <- aDialog(items=list(
                          x=numericItem(NA, eval=TRUE),
                          y=numericItem(NA, eval=TRUE),
                          hyps$instance()
                          ),
                        OK_handler=function(.) {
                          do.call("wilcox.test", .$to_R())
                        }
                        )

ttestDialog$make_gui()
wilcoxDialog$make_gui()                 # shares alt info!


###################################################
### chunk number 20: 
###################################################
i <- numericItem(0, name="item1")
i$get_item1()
i$set_item1(3)
i$get_item1()
try(i$set_item1("c(1,2,3)"))            # fails validation, still stored in model
i$get_item1()


###################################################
### chunk number 21: 
###################################################
i <- numericItem(0, name="item2", eval=TRUE)
i$set_item2("c(1,2,3)")                 # now okay
i$get_item2()
i$to_R()                                # coerced


###################################################
### chunk number 22: 
###################################################
dlg <- aDialog(items=list(
                 x=numericItem(0),
                 y=stringItem("a")
                 ))
dlg$get_x()
dlg$set_y("some string")
dlg$get_y()


###################################################
### chunk number 23: 
###################################################
ig <- anItemGroup(items=list(
                  x=numericItem(1),
                  y=choiceItem("a", values=letters[1:5])
                    )
                  )
ig$get_y()
i <- ig$get_item_by_name("y")
i$get_y()                               # same as above
i$get_values()                          # get values
i$set_values(letters)                   # to set values


###################################################
### chunk number 24: 
###################################################
dlg <- aDialog(items=list(
                 x=numericItem(0),
                 y=numericItem(0)
                 )
               )
dlg1 <- aDialog(items=list(
                  a=numericItem(0)
                  ),
                property_x_value_changed=function(., value, old_value) {
                  .$set_a(.$get_a() + value) # add value to a (assumes numeric)
                }
                )

dlg$add_observer(dlg1)
dlg1$get_a()
dlg$set_x(10)
dlg1$get_a()                            # updated by x


###################################################
### chunk number 25: 
###################################################
dlg <- aDialog(items=list(
                 f=fileItem(""),
                 i=imageItem("",attr=list(size=c(480,480)))
                 ),
               property_f_value_changed=function(., value, old_value) {
                 .$set_i(value)
               },
               buttons="Cancel")
dlg$make_gui()


###################################################
### chunk number 26: 
###################################################
i <- numericItem(0, name="x")
j <- numericItem(1, name="x")
j$get_x()
j$set_model(i)
j$get_x()
i$set_x(10)
j$get_x()


###################################################
### chunk number 27: 
###################################################
mb_l <- list(File=list(
               New=gaction("new", icon="new", handler=function(h,...) print("New")),
               Quit=gaction("quit", icon="quit", handler=function(h,...) dlg$close_gui())
               ))
tb_l <- list(Quit=gaction("quit", icon="quit", handler=function(h,...) dlg$close_gui()))
dlg <- aDialog(items=list(x=stringItem("some value")),
               menu_list=mb_l,
               toolbar_list=tb_l,
               title="Dialog with menu and toolbar")
dlg$make_gui()


###################################################
### chunk number 28: 
###################################################
dlg <- aDialog(items=list(
                 x=numericItem(0)
                 ),
               title="Change x to be able to close",
               buttons="Close",
               Close_handler=function(.) {
                 if(.$get_x() != 0)
                   .$close_gui()
                 })
dlg$make_gui()


###################################################
### chunk number 29: 
###################################################
df <- rbind(
            c("aContainer","Basic container, uses tabular layout"),
            c("aTableLayout","Tabular layout with more than 2 columns"),
            c("aGroup","Box container to pack in children left to right or top to bottom"),
            c("aFrame","Box container with decorative frame"),
            c("anExpandGroup","Box container with trigger to hide"),
            c("aPanedGroup","Two pane container"),
            c("aNotebook", "Notebook container"),
            c("aContext", "Provide context for an item or items")
            )
colnames(df) <- c("Constructor","Description")
cat(booktabs(df,
             colTypes=c("l","p{0.7\\textwidth}"),
             caption="Table of view constructors.",
             label="tab:view-constructors"))


###################################################
### chunk number 30: 
###################################################
dlg <- aDialog(items=list(x=numericItem(1)))
g <- aGroup()                           # define outside view to access later
view <- aContainer("x",g)
dlg$make_gui(gui_layout=view, visible=FALSE) # postpone showing, but create containers
l <- glabel("Look ma, a gWidgets label", cont = g$container) # how to find container
dlg$visible(TRUE)


###################################################
### chunk number 31: 
###################################################
dlg <- aDialog(items=list(x=numericItem(0)))
g <- aGroup(visible_when=function(.) FALSE) # suppress showing
view <- aContainer("x", g)
dlg$make_gui(gui_layout=view)
## now to add to the GUI at g:
ig <- anItemGroup(list(y=stringItem("a string")))
ig$make_gui(container=g)
g$visible_when <- function(.) TRUE
dlg$update_ui()                        


###################################################
### chunk number 32: 
###################################################
m <- getCRANmirrors(all = FALSE, local.only = FALSE)[,c(1,2,4)]
setCran <- function(.,...) {
  URL <- .$get_cran()
  repos <- getOption("repos")
  repos["CRAN"] <- gsub("/$", "", URL[1L])
  options(repos = repos)
  .$close_gui()
}


###################################################
### chunk number 33: 
###################################################
dlg <- aDialog(items=list(
                 cran=choiceItem(value=NA, values=m,
                   show_label=FALSE,  # suppress label
                     attr=list(chosencol=3, size=c(400,500)) #chosencol is URL one, not first
                   )
                 ),
               title="Choose a CRAN Mirror",
               help_string="Click a mirror, then OK, or double click on mirror",
               OK_handler=setCran,                 # OK button click
               property_cran_value_changed=setCran # double click
               )
dlg$make_gui()


###################################################
### chunk number 34: replot
###################################################
  replot <- function(.) {
    l <- .$to_R()
    f <- function(dist, kernel, n, bw,...) {
      y <- switch(dist, "Normal"=rnorm(n), rexp(n))
      plot(density(y,  bw=bw, kernel=kernel), xlim=range(y)+c(-2,2), main="Density example")
      points(y, rep(0,n))
    }
    do.call(f,l)
  }


###################################################
### chunk number 35: 
###################################################
dlg <- aDialog(items=list(
                 dist=choiceItem("Normal", values=c("Normal","Exponential"),
                   show_label=FALSE),
                 kernel=choiceItem("gaussian",
                   values=c("gaussian", "epanechnikov", "rectangular",
                     "triangular", "cosine"),
                   show_label=FALSE),
                 n=choiceItem(50L, as.integer(c(50,100,200,300)),
                   show_label=FALSE),
                 bw=rangeItem(value=1, from=0.05, to=2.00, by=0.05,
                   show_label=FALSE),
                 out=graphicDeviceItem()
                 ),
               help_string="Adjust a parameter to update graphic",
               title="tkdensity through traitr",
               buttons="Cancel",
               model_value_changed=replot)


###################################################
### chunk number 36: 
###################################################
view <- aGroup(aContainer(aFrame("dist", label="Distribution"),
                            aFrame("kernel", label="Kernel"),
                            aFrame("n", label="Sample size"),
                            aFrame("bw", label="Bandwidth")
                            ),
                 "out",
                 horizontal=TRUE)
  dlg$make_gui(gui_layout=view)
  replot(dlg)                           # initial plot


###################################################
### chunk number 37: 
###################################################
i1 <- itemList(items=list(),
               items_names="x",
               item_factory=function(.) numericItem(0)
               )
i1$make_ui(container=gwindow("Basic Use"))


###################################################
### chunk number 38: 
###################################################
## add some items offline
for(i in 1:2) i1$append_item(i1$item_factory())


###################################################
### chunk number 39: 
###################################################
i1$to_R()


###################################################
### chunk number 40: 
###################################################
i2 <- itemList(items=list(),
               items_names="Data frame rows",
               item_factory=function(.) {
                 ig <- anItemGroup(items=list(a=numericItem(0), b=stringItem("")))
                 ig$to_string <- function(.) .$get_b()
                 ig
               })


###################################################
### chunk number 41: 
###################################################
i2$to_R <- function(.) {
  items <- .$get_value()
  if(length(items) == 0) {
    out <- as.data.frame(.$item_factory()$to_R(), stringsAsFactors=FALSE)[0,]
  } else {
    out <- as.data.frame(items[[1]]$to_R(), stringsAsFactors=FALSE)
    if(length(items) > 1) {
      for(i in 2:length(items))
        out[i,] <- items[[i]]$to_R()
    }
  }
  out
}


###################################################
### chunk number 42: 
###################################################
i2$make_ui(container=gwindow("Basic Use to make data frame"))


###################################################
### chunk number 43: 
###################################################
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
                 as.is=trueFalseItem(!default.stringsAsFactors(), 
                   tooltip="Do not convert character to factor"),
                 na.strings=stringItem("NA", tooltip="Strings to be NA", eval=TRUE),
                 nrows=integerItem(-1, tooltip="Max number of rows to read"),
                 skip=integerItem(0, tooltip="Number of lines to skip at header"),
                 check.names=trueFalseItem(TRUE, tooltip="If TRUE ensure names are valid"),
                 fill=trueFalseItem(TRUE, tooltip="Fill unequal length rows if TRUE"),
                 strip.white=trueFalseItem(TRUE),
                 blank.lines.skip=trueFalseItem(TRUE, tooltip="If TRUE, skip blank lines"),
                 comment.char=stringItem("#", tooltip="Comment character"),
                 allowEscapes=trueFalseItem(TRUE, tooltip="C-style escapes read verbatim?"),
                 stringsAsFactors=trueFalseItem(default.stringsAsFactors(), 
                   tooltip="Characters converted to factors"),
                 fileEncoding=stringItem(""),
                 encoding=stringItem("unknown"),
                 ## our things
                 assign.to=stringItem("df", label="Assign to:"),
                 output=tableItem(attr=list(size=c(400,400)), show_label=FALSE),
                 file.type=stringItem("")
                 ),
               title="Read in txt or csv file",
               help_string="Select a file, then adjust parameters."
               )


###################################################
### chunk number 44: 
###################################################
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


###################################################
### chunk number 45: 
###################################################
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


###################################################
### chunk number 46: 
###################################################
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


###################################################
### chunk number 47: 
###################################################
dlg$OK_handler <- function(.) {
  out <- .$do_call("read_file",.$to_R())
  assign.to <- .$get_assign.to()
  if(exists(assign.to, envir=.GlobalEnv)) {
    if(!gconfirm(sprintf("Overwrite variable %s?", assign.to)))
      return()
  }
  assign(assign.to, out, envir=.GlobalEnv)
}


###################################################
### chunk number 48: 
###################################################
dlg$make_gui(gui_layout=view)


###################################################
### chunk number 49: 
###################################################
dlg$property_file_value_changed <- function(., value, old_value) {
  if(file.exists(value)) {
    for(i in c("txt","csv")) {
      if(grepl(paste("\\.",i,sep=""), value))
        .$set_file.type(c(txt="table",csv="csv")[i])
    }
  }
  switch(.$get_file.type(),
         "csv"={.$set_sep(","); .$set_quote('\"')},
         "table"={.$set_output(.$do_call("read_file",.$to_R()))},
         {}
         )

}


###################################################
### chunk number 50: 
###################################################
nms <- names(dlg$get_items())
nms <- setdiff(nms, c("file","file.type","assign.to","output"))
QT <- sapply(nms, function(i) {
  assign(sprintf("property_%s_value_changed",i),
         function(., ...) {
           .$set_output(.$do_call("read_file",.$to_R()))
         },
         envir=dlg)
})


###################################################
### chunk number 51: 
###################################################
m <- mtcars
nms <- names(m)
make_model <- function(i) {
  l <- list(i = integerItem(i))
  for(j in 1:ncol(m)) {
    l[[nms[j]]] <- numericItem(m[i,j])
  }
  l
}


###################################################
### chunk number 52: 
###################################################
dlg <- aDialog(items=make_model(1),
               title="Data frame scroller",
               help_string="Press buttons to scroll through data set",
               buttons=c("<< previous","next >>","SPACE","Cancel"),
               set_row=function(.,i) {
                 if(i < 1)
                   i <- nrow(m)
                 if(i > nrow(m))
                   i <- 1
                 ig <- make_model(as.numeric(i))
                 .$set_model(anItemGroup(items=ig))
               },
               previous_handler=function(.) {
                 i <- as.integer(.$get_i())
                 .$set_row(i-1)
               },
               next_handler=function(.) {
                 i <- .$to_R()$i        # same but different
                 .$set_row(i + 1)
               })


###################################################
### chunk number 53: 
###################################################
dlg$make_gui()


###################################################
### chunk number 54: 
###################################################
model <- aDialog(items=list(
                   a=stringItem(""),
                   b=stringItem("")
                 )
                 )

dlg1 <- aDialog(buttons="Next",
                Next_handler=function(.) {
                  dlg2$make_gui(gui_layout=view2)
                  .$close_gui()
                })
view1 <- aContainer("a", context=model)

dlg2 <- aDialog(buttons = c("Finished"),
                Finished_handler = function(.) {
                  print(model$to_R())
                  .$close_gui()
                })
view2 <- aContainer("b", context=model)

dlg1$make_gui(gui_layout=view1)


###################################################
### chunk number 55: SpotfireExample
###################################################
theDesc <- paste("<b>Spotfire</b>",
                 "The Spotfire web player (http://spotfire.tibco.com)",
                 "has several demos built around a somewhat similar set-up:",
                 "a description page, a data set, a graphic display of the data, and a set",
                 "of controls to filter out the data that is being displayed in the graphic.",
                 "",
                 "This example shows how <b>traitr</b> can be used to make a simple version of such.",
                 "",
                 "Click the <i>Explore</i> tab to begin.",
                 sep="\n")


###################################################
### chunk number 56: dataDisplay
###################################################
theData <- mtcars
makeLabel <- function(nr) sprintf("<b>%s</b> cases",nr)
dataDisplay <- anItemGroup(items=list(
                             data = tableItem(theData, name="data", attr=c(expand=TRUE)),
                             label = labelItem(makeLabel(nrow(theData)), attr=c(markup=TRUE, expand=FALSE))
                             ),
                           attr=c(expand=TRUE)
                           )


###################################################
### chunk number 57: 
###################################################
## synchronize labe with data dimension
dataDisplay$property_data_value_changed <- function(., value, old_value)
  .$set_label(makeLabel(nrow(.$get_data())))


###################################################
### chunk number 58: 
###################################################
dataDisplay$make_default_gui_layout <- function(.) {
  aGroup("data",
         aGroup(labelItem("", attr=c(expand=TRUE)),"label", horizontal=TRUE),
         horizontal=FALSE)
}


###################################################
### chunk number 59: filterBy
###################################################
var <- "cyl"
varLevels <- unique(theData[, var])
var1 <- "wt"
rngvar <- range(theData[, var1])

filters <- anItemGroup(items=c(
                         sapply(varLevels, function(i) {
                           l <- list(trueFalseItem(TRUE, label=as.character(i), editor_style="compact"))
                           names(l) <- paste(var,i, sep=":")
                           l
                         }),
                         
                         labelItem("Weight >"),
                         weight=rangeItem(value=rngvar1[1] - .2, from=rngvar1[1], to=rngvar[2], by=.2,
                           show_label=FALSE, label="Weight >")
                       ))


###################################################
### chunk number 60: 
###################################################
filters$model_value_changed <- function(.) {
  ## var
  vals <- theData[, var]
  tmp <- .$to_R()
  tmp <- tmp[grepl(paste("^", var, sep=""), names(tmp))]
  selected <- names(tmp)[unlist(tmp)]
  selected <- gsub(paste("^", var,":", sep=""), "", selected)
  ind <- vals %in% as.numeric(selected)

  ## var1
  vals <- theData[, var1]
  ind1 <- vals >= .$to_R()$weight
  dlg$update_data(as.logical(ind * ind1))
}


###################################################
### chunk number 61: 
###################################################
## This uses dataset specific values -- need to abstract out?
filters$make_default_gui_layout <- function(.) {
  nms <- names(.$get_items())
  nms <- nms[grepl(paste("^",var, sep=""), nms)]
  aFrame(do.call("aFrame", c(lapply(nms, function(i) i), label="Cylinders")),
         aFrame("weight", label="Weight >"),
                 label="Filter by",
         attr=c(size=c(300,-1)))
}


###################################################
### chunk number 62: 
###################################################
dlg <- aDialog(items=list(
                 Description=labelItem(theDesc, attr=c(markup=TRUE)),
                 gd = graphicDeviceItem(),
                 filters,
                 dataDisplay
                 ),
               title="Spotfire example",
               buttons="Cancel"
               )


###################################################
### chunk number 63: specifyDialog
###################################################
dlg$property_data_value_changed <- function(., value, old_value) 
  .$draw_graphic()


###################################################
### chunk number 64: 
###################################################
dlg$update_data <- function(., index) {
  if(missing(index))
    data <- theData
  else
    data <- theData[index,]
  dataDisplay$set_data(data)
}


###################################################
### chunk number 65: 
###################################################
dlg$draw_graphic <- function(.) {
  data <- dataDisplay$get_data()
  if(nrow(data)) {
    hist(data[,"mpg"])
    rug(data[, "mpg"])
  } else {
    plot.new()
  }
}


###################################################
### chunk number 66: 
###################################################
## Layout for main GUI
lyt <- aNotebook(aNotebookPage("Description",
                               label="About"
                               ),
                 aNotebookPage(aPanedGroup(
                                           aPanedGroup("gd",
                                                       dataDisplay,
                                                       horizontal=FALSE),
                                           filters),
                               label="Explore"
                               )
                 )


###################################################
### chunk number 67: makeGUI
###################################################
w <- loadingAnimation()
dlg$make_gui(gui_layout=lyt)
w$close()


