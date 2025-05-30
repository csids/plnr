% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand_list.R
\name{expand_list}
\alias{expand_list}
\title{Create a cross product of lists}
\usage{
expand_list(...)
}
\arguments{
\item{...}{Named arguments, each containing a vector or list of values to combine}
}
\value{
A list of lists, where each inner list contains one combination of values
from the input arguments
}
\description{
This function creates a cross product of multiple lists, similar to \code{purrr::cross()}
but with a more convenient interface that doesn't require wrapping arguments in an
extra \code{list()}. It's useful for generating combinations of parameters for analysis.
}
\examples{
# Create combinations of parameters
combinations <- plnr::expand_list(
  a = 1:2,
  b = c("a", "b")
)

# View the combinations
str(combinations)

# Compare with purrr::cross
purrr::cross(list(
  a = 1:2,
  b = c("a", "b")
))
}
