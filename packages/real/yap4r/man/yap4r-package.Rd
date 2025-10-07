\name{yap4r-package}
\alias{yap4r-package}
\alias{yap4r}
\docType{package}
\title{
  Calling Prolog from R with YAP4R
}

\author{
V\'{\i}tor Santos Costa e-mail{vscosta@fc.up.pt}
}

\description{
  This package allows R programers to create and query Prolog engines  (currently one), and to query it. It is possible to compile Prolog files, and to share data between R and YAP.
}
\details{
  The first step is to load the package:
  }

\code{
library(yap4r)
}

{next, you create an engine object:}
\code{
y <- new(yap4r)
}

and can query it:

\code{
 \method{y}(query}("between(1,10,X)")
}
\keyword{ prolog yap }
\examples{
\dontrun{
## Optional simple examples of the most important functions
  default:
## These can be in \dontrun{} and \donttest{} blocks.   
# load yap4r
library(yap4r)
# start a new yap engine, y
y <- new(yap4r)
# execute a query, and collect the first solution as a vector of bindings
y$query("between(1,3,X)")
y$more()
y$more()
y$more()
#until it fails
y$more()
#you can ask for more complex queries
y$query("between(1,3,X), Y is 2*X, Z=p(X+Y) ")
#notice that Y is evaluated but Z is just a function
# you can focus on Z:
y$query("between(1,10,X), Y is 2*X, Z=p(X+Y) ")$Z
 }
}
