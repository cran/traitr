\name{loadingAnimation}
\alias{loadingAnimation}
\title{A window to show a loading animation...}
\usage{loadingAnimation(message="<b>Loading...</b>")}
\description{A window to show a loading animation}
\value{An item group instance with a \code{close} method to call to dismiss window}
\arguments{\item{message}{A message to display along with graphic while loading. PANGO markup is okay.}}
\examples{## we call, something happens, then we close
\dontrun{
w <- loadingAnimation()
## .... something long, like dlg$make_gui() ...
w$close()
}}
