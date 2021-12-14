\name{rweyl}
\alias{rweyl}
\title{Random weyl objects}
\description{
Creates random weyl objects: quick-and-dirty examples of Weyl algebra
elements}
\usage{
rweyl(nterms = 3, vals = seq_len(nterms), dim = 3, powers = 0:2)
}
\arguments{
  \item{nterms}{Number of terms in output}
  \item{vals}{Values of coefficients}
  \item{dim}{Dimension of weyl object}
  \item{powers}{Set from which to sample the entries of the index matrix}
}
\value{Returns a weyl object}
\author{Robin K. S. Hankin}
\examples{
rweyl()

rweyl(d=7)
}