% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miter-table.R
\name{miter_table}
\alias{miter_table}
\alias{as_miter_table}
\title{Create a nested table with extra class \code{miter_tbl}}
\usage{
miter_table(x)

as_miter_table(x)
}
\arguments{
\item{x}{A data frame containing all relevant variables.}
}
\value{
A nested tibble with extra class \code{miter_tbl} and the following added
columns:
\itemize{
\item \code{id} could be one or more columns that identify unique ids provided by the
user through \code{group_by()}.
\item \code{data} a list-column with the data provided by user in the argument \code{x} for
each specified group.
}
}
\description{
Create a nested table with extra class \code{miter_tbl}
}
