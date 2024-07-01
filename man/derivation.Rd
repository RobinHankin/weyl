\name{derivation}
\alias{derivation}
\alias{derivations}
\alias{as.der}
\title{Derivations}
\description{

A \dfn{derivation} \eqn{D} of an algebra \eqn{A} is a linear
operator that satisfies \eqn{D(d_1d_2)=d_1D(d_2)+D(d_1)d_2}{omitted},
for every \eqn{d_1,d_2\in A}{omitted}.  If a derivation is of the form
\eqn{D(d)=[d,f]=df-fd}{omitted} for some fixed \eqn{f\in
A}{omitted}, we say that \eqn{D} is an \dfn{inner} derivation.
  
Function \code{as.der()} returns a derivation with
\code{as.der(f)(g)=fg-gf}.

}
\usage{as.der(S)}
\arguments{\item{S}{Weyl object}}
\value{Returns a function, a derivation}
\author{Robin K. S. Hankin}
\examples{

(o <- rweyl(n=2,d=2))
(f <- as.der(o))

d1 <-rweyl(n=1,d=2)
d2 <-rweyl(n=2,d=2)

f(d1*d2) == d1*f(d2) + f(d1)*d2 # should be TRUE

}
