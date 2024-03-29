% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join.R
\name{join_name_abb}
\alias{join_name_abb}
\title{Join Name by Abb}
\usage{
join_name_abb(.data, .abb)
}
\arguments{
\item{.data}{data.frame or tibble}

\item{.abb}{column with state abbreviation}
}
\value{
.data with column .abb replaced with state name
}
\description{
Adds a column with state name joining by a column with state abbreviation
}
\examples{
data('stata')
stata \%>\% join_name_abb(abb)
}
\concept{join}
