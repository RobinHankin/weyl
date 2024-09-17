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
as.weyl(val,d)
is.ok.weyl(M)
}
\arguments{
  \item{M}{A weyl or spray object}
  \item{val,d}{Value and dimension for weyl object}
}
\details{

  To create a weyl object, pass a spray to function \code{weyl()}, as in
  \code{weyl(M)}.  To create a spray object to pass to \code{weyl()},
  use function \code{spray()}, which is a synonym for
  \code{spray::spray()}.

Function \code{weyl()} is the formal creator method; \code{is.weyl()}
tests for weyl objects and \code{is.ok.weyl()} checks for well-formed
sprays.  Function \code{as.weyl()} tries (but not very hard) to infer
what the user intended and return the right thing.

}
\value{Return a weyl or a Boolean}
\author{Robin K. S. Hankin}
\examples{

(W <- spray(matrix(1:36,6,6),1:6))
weyl(W)

as.weyl(15,d=3)

is.ok.weyl(spray(matrix(1:30,5,6)))
is.ok.weyl(spray(matrix(1:30,6,5)))

}
