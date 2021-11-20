#' isNotNull: is object not null
#'
#' @param x an object
#' @return \code{boolean} is object null T/F
#' @examples
#'\dontrun{isNotNull(NULL)}
#'\dontrun{isNotNull(5)}
isNotNull <- function(x)  !is.null(x)

#' outputSectionHeader: creates an outputSectionHeader
#'
#' @param headerName a header name
#' @return \code{character} a character based section headers
outputSectionHeader <- function(headerName) {
  newLine <- "\n"
  decoration <- paste(rep("=", nchar(headerName)), collapse="")
  header <- paste(newLine, headerName, newLine, decoration, newLine, sep="")
  return(header)
}
