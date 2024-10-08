% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_versioned_dir.R, R/utils_io.R
\name{get_latest_output_date_index}
\alias{get_latest_output_date_index}
\title{get the latest index for given an output dir and a date}
\usage{
get_latest_output_date_index(dir, date)

get_latest_output_date_index(dir, date)
}
\arguments{
\item{dir}{[chr] path to directory with versioned dirs}

\item{date}{[chr] character in be YYYY_MM_DD format}
}
\value{
largest version in directory tree or 0 if there are no version OR
the directory tree does not exist

[dbl] largest version in directory tree or 0 if there are no version OR
the directory tree does not exist
}
\description{
directories are assumed to be named in YYYY_MM_DD.VV format with sane
year/month/date/version values.

directories are assumed to be named in YYYY_MM_DD.VV format with sane
year/month/date/version values.
}
\examples{
get_latest_output_date_index("tests/testthat/fixtures/versioned-dirs/nested/1999_09_09", date = "1999_09_09") # expect 2
}
