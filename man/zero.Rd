\name{zero}
\alias{zero}
\alias{empty}
\alias{is.zero}
\alias{is.empty}
\title{The zero operator}
\description{
The zero operator maps any function to the zero function (which maps
anything to zero).  To test for being zero, use \code{spray::is.zero()};
package idiom would be \code{is.zero()}. 
}
\usage{
zero(d)
}
\arguments{
  \item{d}{Integer specifying dimensionality of the weyl object (twice the
    spray arity)}
}
\examples{
a <- rweyl(d=5)
is.zero(a)
is.zero(a-a)
is.zero(a*0)

a == a + zero(dim(a))

}

