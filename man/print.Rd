\name{print.weyl}
\alias{print.weyl}
\alias{print}
\title{Print methods for weyl objects}

\description{

Printing methods for weyl objects follow those for the \pkg{spray}
package, with some additional functionality.

}
\usage{\method{print}{weyl}(x, ...)}
\arguments{
  \item{x}{A weyl object}
  \item{\dots}{Further arguments, currently ignored}
}
\details{
Option \code{polyform} determines whether the object is to be printed in
matrix form or polynomial form: as in the \pkg{spray} package, this
option governs dispatch to either \code{print_spray_polyform()} or
\code{print_spray_matrixform()}.

Option \code{weylvars} controls the variable names by changing the
\code{sprayvars} option which is used in the \pkg{spray} package.  If
\code{NULL} (the default), then sensible values are used: either
\code{[xyz]} if the dimension is three or less, or integers.

If the user sets \code{weylvars}, the print method tries to do the Right
Thing (tm).  If set to \code{c("a","b","c")}, for example, the
generators are named \code{c(" a"," b"," c","da","db","dc")}.  If the
algebra is univariate, the names will be something like \code{d} and
\code{x}.  No checking is performed and if the length is not equal to
the dimension, undesirable behaviour may occur.  For the love of God, do
not use a variable named \dquote{d}.

Note that, as for the \pkg{spray} package, this option has no algebraic
significance: it only affects the print method.

}

\value{Returns a \code{weyl} object.}
\author{Robin K. S. Hankin}
\examples{
a <- rweyl()
print(a)
options(polyform=TRUE)
print(a)
}