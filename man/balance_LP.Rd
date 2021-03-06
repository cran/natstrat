% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balance_LP.R
\name{balance_LP}
\alias{balance_LP}
\title{Linear program that selects which controls to use in order to optimize balance}
\usage{
balance_LP(
  z,
  X,
  importances,
  st,
  st_vals,
  S,
  q_s,
  N,
  solver,
  integer,
  time_limit,
  threads = 1,
  weight_comp = 1
)
}
\arguments{
\item{z}{a factor with the \code{i}th entry equal to the treatment of unit \code{i}.}

\item{X}{a matrix or data frame containing constraints in the columns. The number
of rows should equal the length of \code{z}. Balance is achieved when a constraint
sums to 0, such that numbers closer to 0 are better. When a constraint
does not apply to a particular unit, the entry should be \code{NA}.
This should typically be generated using \code{\link{generate_constraints}()}.}

\item{importances}{a vector with length equal to the number of constraints or columns
in \code{X}. This can be generated using \code{\link{generate_constraints}()} and each nonnegative value
denotes how much to prioritize each constraint, with the default being 1
for all constraints.}

\item{st}{a stratum vector with the \code{i}th entry equal to the
stratum of unit \code{i}. This should have the same order of units and length
as \code{z}.}

\item{st_vals}{the unique stratum levels contained in \code{st}.}

\item{S}{the number of unique stratum levels contained in \code{st}.}

\item{q_s}{a named vector or matrix indicating how many control units are to be selected from each stratum.
If there is one control group and all treated units are desired, this can be a vector; otherwise,
this should have one row per treatment group, where the order of the rows matches the order of
the levels of \code{z}, including the treated level.}

\item{N}{the total number of available controls in the data.}

\item{solver}{a character stating which solver to use to run the linear program.
Options are "Rglpk" (default) or "gurobi". You must have the 'gurobi' package
installed to use the "gurobi" option. If available, this is the recommended solver.}

\item{integer}{a logical stating whether to use a mixed integer programming solver
instead of randomized rounding. Default is \code{FALSE}.}

\item{time_limit}{numeric stating maximum amount of seconds for which the
program is allowed to run before aborting. Default is \code{Inf} for no time limit.}

\item{threads}{The maximum number of threads that should be used. This is only
applicable if \code{solver = 'gurobi'}.}
}
\value{
A list containing two elements:
\describe{
\item{\code{lpdetails}}{The output of either \code{gurobi()} or \code{\link[Rglpk]{Rglpk_solve_LP}()},
except that if \code{gurobi()} is used, the elements \code{objval} and \code{x}
are renamed \code{optimum} and \code{solution}
to be consistent with the output of \code{\link[Rglpk]{Rglpk_solve_LP}()}.}
\item{\code{o}}{The original output of either \code{gurobi()} or \code{\link[Rglpk]{Rglpk_solve_LP}()}.}
}
}
\description{
This linear program is used by \code{\link{optimize_controls}()} to choose which controls
to select.
}
\keyword{internal}
