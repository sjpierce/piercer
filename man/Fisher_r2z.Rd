% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Transform_Functions.R
\name{Fisher_r2z}
\alias{Fisher_r2z}
\title{Fisher's r to z transformation.}
\usage{
Fisher_r2z(r)
}
\arguments{
\item{r}{A numeric value (or vector) for the correlation coefficient(s).}
}
\value{
A numeric value (or vector) for the z-scores.
}
\description{
This function uses Fisher's r to z transformation to convert a
  correlation into a z-score.
}
\details{
This applies Fisher's r to z transformation to convert a correlation
  into a z-score. Numerical values of r close to -1 or 1 result Inf or -Inf,
  respectively. Missing values of r result in NA values. This transformation
  is also called the inverse hyperbolic tangent transformation (often written
  as arctanh), so Fisher_r2z() is a wrapper for the base::atanh() function.
}
\examples{
Fisher_r2z(.3)
Fisher_r2z(c(-1, -.99, NA, 0, .5, .99, 1))

}
\references{
To be added later.
}
\seealso{
\code{\link[base]{atanh}} for the base function used in this
  function. \code{\link{Fisher_z2r}} for the inverse transformation that
  converts z back to r.
}
