\name{weyl}
\alias{weyl}
\alias{as.weyl}
\alias{is.weyl}
\alias{is.ok.weyl}
\title{The algebra and weyl objects}
\description{Basic functions for weyl objects}
\usage{
weyl(M)
is.weyl(M)
is.ok.weyl(M)
}
\arguments{
  \item{M}{A weyl or spray object}
}
\details{
Function \code{weyl()} is the formal creator method; \code{is.weyl()}
tests for weyl objects and \code{is.ok.weyl()} checks for well-formed a 
spray.
}
\value{Return a weyl or a Boolean}
\author{Robin K. S. Hankin}
\examples{

weyl(spray(matrix(1:36,6,6),1:6))

}