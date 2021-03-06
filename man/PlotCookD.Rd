% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLM_functions.R
\name{PlotCookD}
\alias{PlotCookD}
\title{Plot Cook's D values}
\usage{
PlotCookD(x, id.n = 10)
}
\arguments{
\item{x}{A glm() model fit object.}

\item{id.n}{An integer specifying how many observations to label.}
}
\value{
None. This produces a plot via ggplot().
}
\description{
This function plots Cook's D values from a glm() model fit
  against the observation index, and highlights observations with values
  exceeding a recommended cutoff.
}
\details{
=============================================================================

Fox (1997, p. 280) suggested a cutoff value for identifying
  observations with high Cook's D values in a GLM model. Plotting the Cook's
  D values against the observation number (row index) and highlighting the
  values that exceed the cutoff is a quick way to inspect a model fit.
}
\examples{
 m1 <- glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)
 PlotCookD(m1)

}
\references{
Fox, J. (1997). Applied regression analysis, linear models, and
  related methods. Thousand Oaks, CA: Sage Publications.
}
\seealso{
\code{\link{CookDco}} for Cook's D cutoff.
}
