\name{spray}
\alias{spray}
\title{Create spray objects}
\description{Function \code{spray()} creates a sparse array; function
  \code{weyl()} coerces a spray object to a  Weyl object.
}
\usage{
spray(M,x,addrepeats=FALSE)
}
\arguments{
  \item{M}{An integer-valued matrix, the index of the weyl object}
  \item{x}{Numeric vector of coefficients}
  \item{addrepeats}{Boolean, specifying whether repeated rows are to be added}
}
\details{

  The function is discussed and motivated in the \pkg{spray} package.

}
\value{Return a weyl or a Boolean}
\author{Robin K. S. Hankin}
\examples{

(W <- spray(matrix(1:36,6,6),1:6))
weyl(W)

as.weyl(15,d=3)
}
