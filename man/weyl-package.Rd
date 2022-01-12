\name{weyl-package}
\alias{weyl-package}
\docType{package}
\title{
\packageTitle{weyl}
}
\description{
\packageDescription{weyl}
}
\details{\loadmathjax

The DESCRIPTION file:
\packageDESCRIPTION{weyl}
\packageIndices{weyl}

}
\author{
\packageAuthor{weyl}

Maintainer: \packageMaintainer{weyl}
}
\keyword{ package }
\examples{
x <- rweyl(d=1)
y <- rweyl(d=1)
z <- rweyl(d=1)

is.zero(x*(y*z) - (x*y)*z)  # should be TRUE
}
