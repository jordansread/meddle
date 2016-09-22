#' a helper function for missing values
#'
#' creates a missing value filler if needed
#'
#' @param value the actual value
#' @param sub.val the value to sub in if \code{value} is empty
#' @return a modified value according to \code{sub.val}
submissing <- function(value, sub.val = 'NA'){
  ifelse(!is.na(value) & value != '',value,sub.val)
}


#' read and format attributes from delimited file
#'
#' formats a file attribute table into a list appropriate for metadata rendering
#'
#' @param filename the name of the file for attributes
#' @return a list of attributes and values for rendering
#' @keywords internal
#' @export
parse_attributes <- function(filename){
  metrics <- read.table(filename, header = TRUE, sep = ',', stringsAsFactors = FALSE)
  out <- list()
  for (metric in metrics$attribute){
    i <- which(metric == metrics$attribute)[1]
    out[[i]] <- list('attr-label' = metric,
                     'attr-def' = submissing(metrics$attr.def[i]),
                     'attr-defs'=submissing(metrics$attr.defs[i]),
                     'data-min'='NA', # to do: calculate this from data
                     'data-max'='NA', # to do: calculate this from data
                     'data-units'=submissing(metrics$units[i]))
  }
  return(list('attributes'=out))
}
