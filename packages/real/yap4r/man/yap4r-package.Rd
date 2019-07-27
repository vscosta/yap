\name{yap4r-package}
\alias{yap4r-package}
\alias{yap4r}
\docType{package}
\title{
  Calling Prolog from R with YAP4R
}
\description{
  This package allows R programers to create and query Prolog engines  (currently one), and to query it. It is possible to compile Prolog files, and to share data between R and YAP.
}
\details{
  The first step is to load the package:
\usage{
library(yap4r)
}
next, you create an engine object:
\examples{
y <- new(yap4r)
}
and can query it:
\examples{
y$query("between(1,10,X)")
}

The whole program is shown here:
\author{
V\'{\i}tor Santos Costa
}

Maintainer: Your Name <your@email.com>
}
\references{
This optional section can contain literature or other references for
background information.
}
\keyword{ package }
\seealso{
Optional links to other man pages
}
\examples{
\dontrun{
## Optional simple examples of the most important functions
## These can be in \dontrun{} and \donttest{} blocks.   
library(yap4r)
y <- new(yap4r)
y$query("between(1,10,X)")
 }
}
