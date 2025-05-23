% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_self_suff_standard.R
\name{sss_main}
\alias{sss_main}
\title{Fetches the Self Sufficiency Standard data for a specific state}
\usage{
sss_main(county_self_sufficiency_standard, state_name)
}
\arguments{
\item{county_self_sufficiency_standard}{A string representing the name of the county.}

\item{state_name}{A string representing the name of the state.}
}
\value{
A data frame containing data specific to the county extracted from the Self Sufficiency Standard file.
}
\description{
This function scrapes the Self Sufficiency Standard website to find the most recent
Self Sufficiency Standard file link for the specified state, downloads it if not already
present in the \code{DataFiles/RawOutputFiles} directory, and reads the file to extract data
specific to the given county.
}
