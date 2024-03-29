% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msg_prt.R
\name{msg_prt}
\alias{msg_prt}
\title{Control how user-defined messages submit to std_err or std_out}
\usage{
msg_prt(string = "No message supplied", output = "message")
}
\arguments{
\item{string}{[chr] string length = 1}

\item{output}{[chr] output mode: c("message", "print", "both")}
}
\value{
[stderr/stdout] one or both
}
\description{
Change this function's default output mode depending on how you debug / read logs
}
