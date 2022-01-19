\name{drop}
\alias{drop}
\alias{drop,weyl-method}
\title{Drop redundant information}
\description{Coerce constant weyl objects to numeric}
\usage{drop(x)}
\arguments{\item{x}{Weyl object}}
\details{If its argument is a constant weyl object, coerce to numeric.}
\author{Robin K. S. Hankin}
\value{Returns either a length-one numeric vector or its argument, a
  weyl object}
\note{Many functions in the package take \code{drop} as an argument
  which, if \code{TRUE}, means that the function returns a
  \code{drop}ped value.}
\examples{
a <- rweyl() + 67
drop(a)

drop(idweyl(9))

drop(constant(a,drop=FALSE))
}
