\name{ItemGroup}
\alias{ItemGroup}
\title{Base Trait to group items together to form a model.}
\description{Base Trait to group items together to form a model. ItemGroups may be viewed as a model, view and controller
bundled together in a tidy package.}
\details{An item group is a collection of Item instances. These
are specified through the \code{items} property as a list.

ItemGroups implement the observer pattern, so are models and can have observers listen for changes
ItemGroups observe themselves to update the user interface on model changes. One can add other observers if desired.
See the \code{init} method for an example.

If a method \code{model_value_changed} is defined, then it will be called with the ItemGroup instance being
in whenever a property of the model has a new value. The handlers \code{property_NAME_value_changed} is called
when the main value in item NAME is changed. (An item can have several properties, one of which is the main one.)

ItemGroups have a \code{make\_gui} method to make a view of the model. The layout of this GUI can be
specified through its \code{gui\_layout} argument, or by default have a simple table layout. ItemGroup instances
are meant to be embedded into a GUI, so the \code{cont} argument is needed to pass in the desired container.}
