\name{constant}
\alias{constant}
\alias{const}
\alias{constant<-}
\alias{constant.weyl}
\alias{constant<-.weyl}
\title{The constant term}
\description{
The \dfn{constant} of a \code{weyl} object is the coefficient of the
term with all zeros.
}
\usage{
constant(x, drop = TRUE)
constant(x) <-  value
}
\arguments{
  \item{x}{Object of class \code{weyl}}
  \item{drop}{Boolean with default \code{TRUE} meaning to return the
    value of the coefficient, and \code{FALSE} meaning to return the
    corresponding Weyl object}
  \item{value}{Constant value to replace existing one}
}
\value{Returns a numeric or weyl object}
\author{Robin K. S. Hankin}
\note{
The \code{constant.weyl()} function is somewhat awkward because it has
  to deal with the difficult case where the constant is zero and
  \code{drop=FALSE}. 
}
\examples{
a <- rweyl()+5
constant(a)
constant(a,drop=FALSE)

constant(a) <- 0
constant(a)
constant(a,drop=FALSE)

constant(a+66) == constant(a) + 66
}