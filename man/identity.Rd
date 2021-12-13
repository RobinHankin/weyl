\name{identity}
\alias{identity}
\alias{id}
\alias{idweyl}
\alias{as.id}
\alias{is.id}
\alias{as.id.weyl}
\alias{is.id.weyl}
\alias{as.one.weyl}
\alias{is.identity}
\alias{as.identity}
\title{The identity operator}
\description{
The identity operator maps any function to itself.
}
\usage{
idweyl(d)
\method{as.id}{weyl}(S)
is.id(S)
}
\arguments{
  \item{d}{Integer specifying dimensionality of the weyl object (twice the
    spray arity)}
  \item{S}{A weyl object}
}
\note{The identity function cannot be called \dQuote{\code{id()}}
  because then \proglang{R} would not know whether to create a
  \code{spray} or a \code{weyl} object.}

\examples{

idweyl(7)

a <- rweyl(d=5)
is.id(a)
is.id(1+a-a)

a == a*1
a == a*as.id(a)

}
