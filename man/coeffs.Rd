\name{coeffs}
\alias{index}
\alias{value}
\alias{values}
\alias{coeff}
\alias{coeffs}
\alias{coeffs<-}
\alias{coeffs.weyl}
\alias{coeffs<-.weyl}
\alias{coeffs,weyl-method}
\alias{value}
\alias{value<-}
\alias{value.weyl}
\alias{value<-.weyl}
\alias{value,weyl-method}
\alias{coeffs<-,weyl-method}
\title{Manipulate the coefficients of a weyl object}
\description{
Manipulate the coefficients of a weyl object.  The coefficients are
\code{disord} objects.
}
\usage{
coeffs(S) <- value
}
\arguments{
  \item{S}{A weyl object}
  \item{value}{Numeric}
}
\details{

To access coefficients of a weyl object \code{S}, use
\code{spray::coeffs(S)} [package idiom is \code{coeffs(S)}].  Similarly
to access the index matrix use \code{index(S)}.

The replacement method is package-specific; use \code{coeffs(S)
<-value}.

}
\value{Extraction methods return a \code{disord} object (possibly
  \code{drop}ped); replacement methods return a \code{weyl} object.}
\author{Robin K. S. Hankin}
\examples{

(a <- rweyl(9))
coeffs(a)
coeffs(a)[coeffs(a)<3] <- 100
a
}
