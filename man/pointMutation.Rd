% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selection.R
\name{pointMutation}
\alias{pointMutation}
\title{pointMutation}
\usage{
pointMutation(solution, functionNodeStructure)
}
\arguments{
\item{solution}{the solution to be mutated}

\item{functionNodeStructure}{the parameters used when creating functionNodes}
}
\value{
the solution after mutation
}
\description{
Peforms point mutation on the solution. It changes up to 5% of the genes
and can change either the function used, or the inputs used.
}
