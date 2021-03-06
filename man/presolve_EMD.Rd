% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/presolve_EMD.R
\name{presolve_EMD}
\alias{presolve_EMD}
\title{Solve the earthmover's distance problem}
\usage{
presolve_EMD(S, desired_qs, max_s, strata_dist_flat)
}
\arguments{
\item{S}{the total number of strata.}

\item{desired_qs}{a named vector containing the number of controls desired in
each stratum with names matching the strata names.}

\item{max_s}{a vector containing the maximum number of controls that should
be selected in each stratum. The order of the strata should match that of \code{desired_qs}.}

\item{strata_dist_flat}{a flattened distance matrix between the strata.}
}
\value{
A named vector with names identical to those of \code{desired_qs} and
elements containing the number of controls to select from the given stratum.
}
\description{
Determine how many controls should be chosen from each stratum to minimize
the distance between the strata of the chosen controls and those that were desired.
Used within \code{\link{generate_qs}()}.
}
\keyword{internal}
