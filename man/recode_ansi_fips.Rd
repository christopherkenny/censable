% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.R
\name{recode_ansi_fips}
\alias{recode_ansi_fips}
\title{Recode ANSI by FIPS}
\usage{
recode_ansi_fips(.data, .fips)
}
\arguments{
\item{.data}{data.frame or tibble}

\item{.fips}{column with state fips}
}
\value{
.data with column .fips replaced with state ansi
}
\description{
Replaces state fips with state ansi
}
\examples{
data('stata')
stata \%>\% recode_ansi_fips(fips)
}
\concept{recode}
