\name{grade}
\alias{grade}
\alias{symbol_map}
\alias{grade<-}
\alias{grades}
\title{The grade of a weyl object}
\description{\loadmathjax

  The \dfn{grade} of a homogeneous term of a Weyl algebra is the sum of
  the powers.  Thus the grade of
  \mjeqn{4xy^2\partial_x^3\partial_y^4}{omitted} is \mjseqn{1+2+3+4=10}.

  The functionality documented here closely follows the equivalent in
  the \CRANpkg{clifford} package.

  Coutinho calls this the \dfn{symbol map}. 
}
\usage{
grade(C, n, drop=TRUE)
grade(C,n) <- value
grades(x)
}
\arguments{
  \item{C,x}{Weyl object}
  \item{n}{Integer vector specifying grades to extract}
  \item{value}{Replacement value, a numeric vector}
  \item{drop}{Boolean, with default \code{TRUE} meaning to coerce a
    constant operator to numeric, and \code{FALSE} meaning not
    to}
}
\details{

Function \code{grades()} returns an (unordered) vector specifying the
grades of the constituent terms.  Function \code{grades<-()} allows
idiom such as \code{grade(x,1:2) <- 7} to operate as expected [here to
set all coefficients of terms with grades 1 or 2 to value 7].

Function \code{grade(C,n)} returns a Weyl object with just the
elements of grade \code{g}, where \code{g \%in\% n}.

The zero grade term, \code{grade(C,0)}, is given more naturally by
\code{constant(C)}.
}
\value{Integer vector or weyl object}
\author{Robin K. S. Hankin}
\examples{

a <- rweyl(30)

grades(a)
grade(a,1:4)
grade(a,5:9) <- -99
a
}
