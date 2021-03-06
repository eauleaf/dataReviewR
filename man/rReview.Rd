% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rc_rReview.R
\name{rReview}
\alias{rReview}
\title{Create distribution and frequency data for each column of a data frame}
\usage{
rReview(df, roundDigits = NA, trimChars = FALSE, clean_var_names = TRUE)
}
\arguments{
\item{df}{data frame. The data object. dataReviewR will attempt to coerce all data objects to data frames.}

\item{roundDigits}{Integer. If NA, numerics are not rounded before comparison. If specified, numerics are
rounded to the specified number of decimal places using \link[base]{round}.}

\item{trimChars}{Boolean. If true, strings and factors have whitespace trimmed before comparison.}

\item{clean_var_names}{User defined boolean indicating whether to clean the variable names using janitor::clean_names, DEFAULT = T}
}
\value{
An dataReviewR object.
An S3 object containing details of the comparison between the two data objects. Can be used with \link{summary},
\link{print} and \link{saveReport}
}
\description{
Create distribution and frequency data for each column of a data frame and produce a dataReviewR object containing
details of the frequency of variables and summary stats for each column of the data. See \code{vignette("dataReviewR")} for more details.
}
\seealso{
Other dataReviewR.functions: 
\code{\link{saveReport}()},
\code{\link{summary.dataReviewRobject}()}
}
\concept{dataReviewR.functions}
