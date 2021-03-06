% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pd_prepareData.R
\name{prepareData}
\alias{prepareData}
\title{prepareData Prepares data for review summaries in 1 stage.
1. coerce data}
\usage{
prepareData(df, trimChars = TRUE, clean_var_names = TRUE)
}
\arguments{
\item{df}{data frame. The data object. dataReviewR will attempt to coerce all data objects to data frames.}

\item{trimChars}{Boolean. If true, strings and factors have whitespace trimmed before comparison.}

\item{clean_var_names}{User defined boolean indicating whether to clean the variable names using janitor::clean_names, DEFAULT = T}
}
\value{
\code{dataReviewRObject} containing details of the comparison
}
\description{
prepareData Prepares data for review summaries in 1 stage.
1. coerce data
}
