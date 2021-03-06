% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population.R
\name{makeFunctionNode}
\alias{makeFunctionNode}
\title{makeFunctionNode}
\usage{
makeFunctionNode(chromoID, validInputs, functionSet)
}
\arguments{
\item{chromoID}{the unique ID of the node}

\item{validInputs}{a vector containing the valid input chromoIDs}

\item{functionSet}{the functions to choose from}
}
\value{
the node created
}
\description{
Creates a valid function node using the chromoID and value passed as a
parameter. A random function is chosen from the functionSet and random inputs
are chosen to satisfy the arity of the chosen function.
}
