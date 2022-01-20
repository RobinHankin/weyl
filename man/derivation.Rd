\name{derivation}
\alias{derivation}
\alias{derivations}
\alias{as.der}
\title{Derivations}
\description{\loadmathjax

A \dfn{derivation} \mjseqn{D} of an algebra \mjseqn{A} is a linear
operator that satisfies \mjeqn{D(d_1d_2)=d_1D(d_2)+D(d_1)d_2}{omitted},
for every \mjeqn{d_1,d_2\in A}{omitted}.  If a derivation is of the form
\mjeqn{D(d)=[d,f]=df-fd}{omitted} for some fixed \mjeqn{f\in
A}{omitted}, we say that \mjseqn{D} is an \dfn{inner} derivation.
  
Function \code{as.der()} returns a derivation with
\code{as.der(f)(g)=fg-gf}.

}
\usage{as.der(S)}
\arguments{\item{S}{Weyl object}}
\value{Returns a function, a derivation}
\author{Robin K. S. Hankin}
\examples{

o <- rweyl(n=2,d=2)
f <- as.der(o)

d1 <-rweyl(n=1,d=2)
d2 <-rweyl(n=2,d=2)

f(d1*d2) == d1*f(d2) + f(d1)*d2 # should be TRUE

}