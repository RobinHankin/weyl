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

Option \code{weylvars} controls the variable names by changing
\code{sprayvars} option which is used in the \pkg{spray} package.  If
\code{weylvars} is set to \code{c("a","b","c")}, the generators are
named \code{c("da","db","dc","a","b","c")}.  I am not entirely happy
with this aspect of the package.  Note that, as for the \pkg{spray}
package, this option has no algebraic significance: it only affects the
print method.
}

\value{Returns a \code{weyl} object.}
\author{Robin K. S. Hankin}
\examples{
a <- rweyl()
print(a)
options(polyform=TRUE)
print(a)
}