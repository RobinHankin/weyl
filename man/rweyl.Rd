\name{rweyl}
\alias{rweyl}
\alias{rweyll}
\alias{rweylll}
\alias{rweyl1}
\title{Random weyl objects}
\description{
Creates random weyl objects: quick-and-dirty examples of Weyl algebra
elements}
\usage{
rweyl(nterms = 3, vals = seq_len(nterms), dim = 3, powers = 0:2)
rweyll(nterms = 15, vals = seq_len(nterms), dim = 4, powers = 0:5)
rweylll(nterms = 50, vals = seq_len(nterms), dim = 8, powers = 0:7)
}
\arguments{
  \item{nterms}{Number of terms in output}
  \item{vals}{Values of coefficients}
  \item{dim}{Dimension of weyl object}
  \item{powers}{Set from which to sample the entries of the index matrix}
}
\details{
Function \code{rweyl()} creates a smallish random Weyl object;
\code{rweyll()} and \code{rweylll()} create successively more
complicated objects.

Function \code{rweyl1()} returns a one-dimensional Weyl object.

}
  
\value{Returns a weyl object}
\author{Robin K. S. Hankin}
\examples{
rweyl()
rweyll()
rweyl(d=7)

options(polyform = TRUE)
rweyl1()
options(polyform = FALSE) # restore default

}
