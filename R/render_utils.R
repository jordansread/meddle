

#' read and format attributes from delimited file or data.frame
#'
#' formats an attribute table into a list appropriate for metadata rendering
#'
#' @param x a file name for attributes file or a data.frame from \code{\link{read_attr_file}}
#' @return a list of attributes and values for rendering
#' @keywords internal
#' @export
as.attr_list <- function(x){
  if (is(x, 'character') && file.exists(x)){
    x <- read_attr_file(x)
  }
  out <- list()
  for (i in 1:nrow(x)){
    out[[i]] <- list(x[i,])
  }
  return(list('attributes'=out))
}

