#' Summarizing RReview Output
#'
#' @family dataReviewR.functions
#' @param object an dataReviewR object, usually a result of a call to \code{\link{rReview}}.
#' @param mismatchCount Integer. How many mismatches to include in tables
#' @param ... Passes any additional arguments (not used in current version)
#' @return The function summary.dataReviewR computes and returns a list of summary details from the dataReviewR output given in \code{object} containing
#' \item{datanameA}{name of the first dataframe in the review call}
#' \item{datanameB}{name of the second dataframe in the review call}
#' \item{nrowA}{the number of rows in \code{datanameA}}
#' \item{nrowB}{the number of rows in \code{datanameB}}
#' \item{version}{the version of \code{\link{rReview}} used to generate the dataReviewR object \code{object}}
#' \item{runtime}{the date and time the dataReviewR object \code{object} was created}
#' \item{rversion}{the version of R used}
#' \item{datasetSummary}{a data frame containing the meta data information on \code{datanameA} and \code{datanameB}}
#' \item{ncolCommon}{the number of columns of the same name contained in both \code{datanameA} and \code{datanameB}}
#' \item{ncolInAOnly}{the number of columns only in \code{datanameA}}
#' \item{ncolInBOnly}{the number of columns only in \code{datanameB}}
#' \item{ncolID}{the number of columns used to match rows in \code{datanameA} and \code{datanameB} }
#' \item{typeMismatch}{a data frame detailing which columns in both \code{datanameA} and \code{datanameB} have different class types}
#' \item{typeMismatchN}{the number of columns with different variable types}
#' \item{nrowCommon}{the number of rows with matching ID columns in both \code{datanameA} and \code{datanameB}}
#' \item{nrowInAOnly}{the number of rows with non matching ID columns in \code{datanameA}}
#' \item{nrowInBOnly}{the number of rows with non matching ID columns in \code{datanameB}}
#' \item{nrowSomeUnequal}{the number of matched rows where at least one value is unequal}
#' \item{nrowAllEqual}{the number of matched rows where all values are equal}
#' \item{ncolsAllEqual}{the number of matched columns where all values are equal}
#' \item{ncolsSomeUnequal}{the number of matched columns where at least one value is unequal}
#' \item{colsWithUnequalValues}{a data frame detailing the mismatches for each matched column}
#' \item{nrowNAmisMatch}{the number of matched numeric rows that contain a NA}
#' \item{maxDifference}{the maximum difference between numeric columns from all matched columns}
#' @export
summary.dataReviewRobject <- function(object, mismatchCount = 5, ...){

  if(!is.numeric(mismatchCount) | mismatchCount < 0) {
    stop("mismatchCount must be a positive number")
  }

  # For Column mismatch details...
  sampleIfPossible <- function(X, size) {
    sampSize = min(size, nrow(X))
    sample_n(X, sampSize)
  }


  # This can be slow  for large files - let the user know its running
  message('dataReviewR is generating the summary...')

  ans <- list()

  #############################################################################


  ans$dataname <- object$meta$df$name
  ans$nrowdf <- object$meta$df$rows

  # Was any rounding conducted?
  ans$rounding <- FALSE
  ans$roundDigits <- 0

  if (is.na(object$meta$roundDigits)) {
    # No rounding was performed
    # Leave as FALSE
  } else {
    # Rounding was performed
    ans$rounding <- TRUE
    ans$roundDigits <- as.numeric(object$meta$roundDigits)
  }


  # Allocate version number for the dataReviewR package to ans$version
  ans$version <- packageVersion("dataReviewR")

  # Allocate time run to ans$runtime
  ans$runtime <- object$meta$runTimestamp

  # Allocate R version to ans$Rversion
  ans$rversion <- R.version.string

  ans$datasetSummary <- as.data.frame(matrix(c(object$meta$df$name, object$meta$df$rows,object$meta$df$cols),
                        ncol=3, nrow=2, byrow= TRUE))
  names(ans$datasetSummary) <- c("Dataset Name", "Number of Rows",
                                 "Number of Columns")

  #############################################################################


  #############################################################################

  class(ans) <- "summary.dataReviewRobject"

  ans

}
