#' Printing summaryRreview Output
#'
#' @param x an object of class "summary.dataReviewRobject", usually a result of a call to
#' \code{\link{summary.dataReviewRobject}}.
#' @param ... Additional arguments passsed on to \code{\link{createTextSummary}}
#' @export
print.summary.dataReviewRobject <- function(x,   ...) {
  y <- createTextSummary(x, ...)
  invisible(y)
}
