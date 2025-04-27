\name{zero}
\alias{zero}
\alias{empty}
\alias{is.zero}
\alias{is.empty}
\title{The zero operator}
\description{

  The zero operator maps any function to the zero function (which maps
  anything to zero).  To test for something being zero, use
  \code{spray::is.zero()}; package idiom would be \code{is.zero()}.

  Function \code{zero()} takes a single argument which is interpreted as
  the dimension of the result.  So \code{zero(dim(a))} returns
  \code{a*0} (but is faster).
  
}
\usage{zero(d)}
\arguments{
  \item{d}{Integer specifying dimensionality of the weyl object (twice the
    spray arity)}
}
\value{A weyl object corresponding to the zero operator (or a Boolean
  for \code{is.zero()})}
\examples{
(a <- rweyl(d=5))
is.zero(a)
is.zero(a-a)
is.zero(a*0)

a == a + zero(dim(a))

zero(8)

}

