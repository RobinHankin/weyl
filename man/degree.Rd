\name{degree}
\alias{deg}
\alias{degree}
\title{The degree of a \code{weyl} object}
\description{

  The \dfn{degree} of a monomial weyl object
  \eqn{x^a\partial^b}{omitted} is defined as \eqn{a+b}.  The degree
  of a general weyl object expressed as a linear combination of
  monomials is the maximum of the degrees of these monomials.  Following
  Coutinho we have:

  \itemize{
  \item
  \eqn{\mathrm{deg}(d_1+d_2)\leq\max(\mathrm{deg}(d_1)+
    \mathrm{deg}(d_2))}{omitted}
  \item
  \eqn{\mathrm{deg}(d_1d_2) = \mathrm{deg}(d_1)+
    \mathrm{deg}(d_2)}{omitted}
  \item
  \eqn{\mathrm{deg}(d_1d_2-d_2d_1)\leq\mathrm{deg}(d_1)+
    \mathrm{deg}(d_2)-2}{omitted}
}
}
\usage{deg(S)}
\arguments{\item{S}{Object of class \code{weyl}}}
\value{Nonnegative integer (or \eqn{-\infty}{-Inf} for the zero Weyl
  object)}
\author{Robin K. S. Hankin}
\note{The degree of the zero object is conventionally
  \eqn{-\infty}{-Inf}.
}
\examples{

(a <- rweyl())
deg(a)

d1 <- rweyl(n=2)
d2 <- rweyl(n=2)

deg(d1+d2) <= deg(d1) + deg(d2)
deg(d1*d2) == deg(d1) + deg(d2)
deg(d1*d2-d2*d1) <= deg(d1) + deg(d2) -2

}


