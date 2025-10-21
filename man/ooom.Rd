\name{ooom}
\alias{ooom}
\title{One over one minus}
\description{
  Uses Taylor's theorem to give one over one minus a Weyl object
  }
\usage{
ooom(W, n)
}
\arguments{
  \item{W}{Weyl object}
  \item{n}{Order of expansion}

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{horner}}}
\examples{
ooom(x+d, 4)

W <- x + x*d
ooom(W, 4)*(1-W) == 1-W^5

}
