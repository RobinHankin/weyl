\name{horner}
\alias{horner}
\title{Horner's method}
\description{Horner's method}
\usage{
horner(W,v)
}
\arguments{
  \item{W}{Weyl object}
  \item{v}{Numeric vector of coefficients}
}
\details{

  Given a formal polynomial

  \deqn{p(x) = a_0 +a_1+a_2x^2+\cdots + a_nx^n}{omitted; see PDF}

  it is possible to express \eqn{p(x)} in the algebraically equivalent
  form

  \deqn{p(x) = a_0 + x\left(a_1+x\left(a_2+\cdots + x\left(a_{n-1} +xa_n
\right)\cdots\right)\right)}{omitted; see PDF}

which is much more efficient for evaluation, as it requires only \eqn{n}
multiplications and \eqn{n} additions, and this is optimal. 
}
  
\author{Robin K. S. Hankin}
\seealso{\code{\link{ooom}}}
\examples{
horner(x, 1:5)
horner(x+d, 1:3)

2+x+d |> horner(1:3) |> horner(1:2)

}
\keyword{symbolmath}
