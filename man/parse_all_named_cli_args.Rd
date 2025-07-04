% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_all_named_cli_args.R
\name{parse_all_named_cli_args}
\alias{parse_all_named_cli_args}
\title{Parse command-line arguments to global environment}
\usage{
parse_all_named_cli_args(
  required_args = NULL,
  trailingOnly = TRUE,
  assign_logical = TRUE,
  assign_integer = TRUE,
  assign_NA = TRUE,
  assign_NULL = TRUE,
  split_comma_str = TRUE,
  allow_rebinding = TRUE,
  assignment_env = globalenv(),
  startup_msg = "Starting arg parser."
)
}
\arguments{
\item{required_args}{[list] (optional; default = NULL) Named list of required
arguments to parse, with data types if desired. If you do not wish to check
data types, supply `NA`.  e.g. `list(arg1 = "character", arg2 = NA)`.
**NOTE:** Parsed args are all "character" by default. If you wish the
parser to assign integer or logical data types, set `assign_integer` or
`assign_logical` to `TRUE`, and then you may check those types as well.}

\item{trailingOnly}{[lgl] if TRUE, only the trailing arguments are used -
passed to `commandArgs()`}

\item{assign_logical}{[lgl] if TRUE, assign true/false in parsed args to
`TRUE`/`FALSE`}

\item{assign_integer}{[lgl] if TRUE, assign solely numeric args to integer
e.g. 5 to `5L`}

\item{assign_NA}{[lgl] if TRUE, assign 'NA' in parsed args to `NA`}

\item{assign_NULL}{[lgl] if TRUE, assign 'NULL' in parsed args to `NULL`}

\item{split_comma_str}{[lgl] if TRUE, split comma-separated strings into
vectors}

\item{allow_rebinding}{[lgl] if TRUE, allow re-binding of variables in the
chosen environment (i.e. lockBinding was already used - unlock and re-lock
all passed args, if relevant)}

\item{assignment_env}{[env] which environment to assign CLI arguments to as
named variables}
}
\value{
[named list] named list of CLI arguments
}
\description{
Add an arbitrary number of CLI args to a parser (default trailing args only),
then assign to a chosen environment (default `globalenv()`).  By default,
automatically assign logical and integer data types when they are detected
among CLI arguments, e.g. 'true' is assigned as `TRUE` and 5 is assigned as
`5L`.
}
\details{
This does not take advantage of argument defaults or types.  See
https://github.com/trevorld/r-argparse for documentation.
}
\examples{
\dontrun{
cli_args <- parse_all_named_cli_args(
   trailingOnly   = TRUE,
   assign_logical = TRUE,
   assign_integer = TRUE,
   assign_NA      = TRUE,
   assign_NULL    = TRUE,
   assignment_env = globalenv()
)
}
}
\seealso{
Other job_submission: 
\code{\link{submit_job}()}
}
\concept{job_submission}
