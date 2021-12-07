\name{identity}
\alias{identity}
\alias{id}
\alias{as.id}
\alias{is.id}
\alias{is.identity}
\alias{as.identity}
\title{The identity operator}
\description{
The identity operator maps any function to itself.
}
\usage{
id(d)
as.id(S)
is.id(S)
}
\arguments{
  \item{d}{Integer specifying dimensionality of the weyl object (twice the
    spray arity)}
  \item{S}{A weyl object}
}
\examples{

id(7)

a <- rweyl(d=5)
is.id(a)
is.id(1+a-a)

a == a*1
a == a*as.id(a)

}
