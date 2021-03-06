% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/out_summaryrreviewobject.R
\name{summary.dataReviewRobject}
\alias{summary.dataReviewRobject}
\title{Summarizing RReview Output}
\usage{
\method{summary}{dataReviewRobject}(object, mismatchCount = 5, ...)
}
\arguments{
\item{object}{an dataReviewR object, usually a result of a call to \code{\link{rReview}}.}

\item{mismatchCount}{Integer. How many mismatches to include in tables}

\item{...}{Passes any additional arguments (not used in current version)}
}
\value{
The function summary.dataReviewR computes and returns a list of summary details from the dataReviewR output given in \code{object} containing
\item{datanameA}{name of the first dataframe in the review call}
\item{datanameB}{name of the second dataframe in the review call}
\item{nrowA}{the number of rows in \code{datanameA}}
\item{nrowB}{the number of rows in \code{datanameB}}
\item{version}{the version of \code{\link{rReview}} used to generate the dataReviewR object \code{object}}
\item{runtime}{the date and time the dataReviewR object \code{object} was created}
\item{rversion}{the version of R used}
\item{datasetSummary}{a data frame containing the meta data information on \code{datanameA} and \code{datanameB}}
\item{ncolCommon}{the number of columns of the same name contained in both \code{datanameA} and \code{datanameB}}
\item{ncolInAOnly}{the number of columns only in \code{datanameA}}
\item{ncolInBOnly}{the number of columns only in \code{datanameB}}
\item{ncolID}{the number of columns used to match rows in \code{datanameA} and \code{datanameB} }
\item{typeMismatch}{a data frame detailing which columns in both \code{datanameA} and \code{datanameB} have different class types}
\item{typeMismatchN}{the number of columns with different variable types}
\item{nrowCommon}{the number of rows with matching ID columns in both \code{datanameA} and \code{datanameB}}
\item{nrowInAOnly}{the number of rows with non matching ID columns in \code{datanameA}}
\item{nrowInBOnly}{the number of rows with non matching ID columns in \code{datanameB}}
\item{nrowSomeUnequal}{the number of matched rows where at least one value is unequal}
\item{nrowAllEqual}{the number of matched rows where all values are equal}
\item{ncolsAllEqual}{the number of matched columns where all values are equal}
\item{ncolsSomeUnequal}{the number of matched columns where at least one value is unequal}
\item{colsWithUnequalValues}{a data frame detailing the mismatches for each matched column}
\item{nrowNAmisMatch}{the number of matched numeric rows that contain a NA}
\item{maxDifference}{the maximum difference between numeric columns from all matched columns}
}
\description{
Summarizing RReview Output
}
\seealso{
Other dataReviewR.functions: 
\code{\link{rReview}()},
\code{\link{saveReport}()}
}
\concept{dataReviewR.functions}
