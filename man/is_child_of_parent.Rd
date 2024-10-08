% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/children_of_parents.R
\name{is_child_of_parent}
\alias{is_child_of_parent}
\title{Helper function for children_of_parents.}
\usage{
is_child_of_parent(parent_id, path_to_top_parent)
}
\arguments{
\item{parent_id}{[int] Location ID of parent to test}

\item{path_to_top_parent}{[character] String of path to top parent from hierarchy}
}
\value{
[lgl] TRUE if parent_id is in path_to_top_parent
}
\description{
Given a single parent_id and a path_to_top_parent,
returns TRUE if that parent_id is in the path.
}
