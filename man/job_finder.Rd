% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_metadata_shell.R
\name{job_finder}
\alias{job_finder}
\title{Find cluster jobs for a user}
\usage{
job_finder(system_user_name, jobname_filter, cluster_type = "slurm")
}
\arguments{
\item{system_user_name}{[chr] string identifying user on the cluster}

\item{jobname_filter}{[regex] filter the user's jobs to include this string}

\item{cluster_type}{[chr] allows methods by cluster type, if multiple are
applicable - "slurm" uses `squeue`}
}
\value{
[data.frame] long by jobid, wide by jobid and jobname
}
\description{
Filters to jobs with State = RUNNING You can filter jobs to a string match
(grepl()).
}
