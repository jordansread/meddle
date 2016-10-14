#' append replace list
#'
#' merge multiple lists with replacement
#'
#' @param list0 the list to append to (w/ replacement)
#' @param \dots additional lists to add to or replace elements in list0
#' @return the merged list with later elements in \dots replacing earlier ones
#' @details
#' order matters for ... arguments. The LAST argument will overwrite anything that proceeds it.
#' That means that \code{append_list_replace(list(dog='Larry'), list(dog='Cindy'))}
#' will use \code{dog='Cindy'}.
append_list_replace <- function(list0, ...){
  new.lists <- c(...)
  if (length(new.lists) < 1){
    return(list0)
  } else {
    list.out <- list0
    for (i in 1:length(new.lists)){
      if (is.null(names(new.lists[i]))){
        stop('currently, unnamed lists are not supported')
      }
      list.out[names(list.out) %in% names(new.lists[i])] <- NULL
      list.out <- append(list.out, new.lists[i])
    }
  }

  return(list.out)
}



as.template <- function(x){
  stopifnot(is.character(x))
  if (file.exists(x)){
    x <- paste(readLines(x),collapse = '\n')
  }
  return(x)
}

as.template.connection <- function(x){

}
