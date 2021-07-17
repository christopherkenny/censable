% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_acs.R, R/memoise.R
\name{build_acs}
\alias{build_acs}
\alias{mem_build_acs}
\title{Build Data from the Decennial Census}
\usage{
build_acs(
  geography,
  state,
  county,
  geometry = TRUE,
  year = 2010,
  survey = "acs5"
)

mem_build_acs(
  geography,
  state,
  county,
  geometry = TRUE,
  year = 2010,
  survey = "acs5"
)
}
\arguments{
\item{geography}{Required. The geography level to use.}

\item{state}{Required. Two letter state postal code.}

\item{county}{Optional. Name of county.  If not provided, returns blocks for the entire state.}

\item{geometry}{Defaults to TRUE. Whether to return the geometry or not.}

\item{year}{year, must be 2000, 2010, or 2020 (after August 2021)}

\item{survey}{whether the get estimates from the 5-year ('acs5'), 3-year ('acs3'),
or 1-year ('acs1') survey. Default is 'acs5'.}
}
\value{
tibble with observations for each observation of the geography in the state
or county. Data includes 2 sets of columns for each race or ethnicity category:
population (pop) and voting age population (vap)
}
\description{
Creates a dataset, using the decennial census information, with the
standard variables used for redistricting. Creates a stable base for getting
data from tidycensus for common calls in redistricting.
}
\examples{
\dontrun{
# uses the Census API
tb <- build_dec(geography = 'block', state = 'NY', county = 'Rockland', geometry = TRUE)
}
}
\concept{build}