#' create a skeleton file (or safely append to existing) for attributes
#'
#' create skeleton from object
#'
#' @param object a regognized R object or vector of file names that can be read in to a recognized object class
#' @param attr.file a file to write the attribute skeleton to
#' @param \dots additional parameters passed to methods
#' @export
attribute_skeleton <- function(object, attr.file, ...){
  UseMethod("attribute_skeleton")
}

#' @keywords internal
#' @export
attribute_skeleton.data.frame <- function(object, attr.file, ...){
  existing.attrs <- read_attr_file(attr.file)
  obj.atts <- names(object)
  # This is a work in progress
  stop('method not implemented')
}

#' attribute skeleton for an assumed file or files
#'
#' will attempt to figure out the content, read in the file(s) and then
#' pass to appropriate method
#'
#' @keywords internal
#' @export

attribute_skeleton.character <- function(object, attr.file, ...){
  if (missing(attr.file)){
    attr.file <- as.attr_file(object)
  }
  # attempt to read in the file based on extension
  stop('method not implemented')
}

#' @importFrom tools file_path_sans_ext
as.attr_file <- function(filename){
  paste0(tools::file_path_sans_ext(filename[1]), '_metadata.csv')
}
#' @keywords internal
#' @export
attribute_skeleton.SpatialPointsDataFrame <- function(object, attr.file, ...){
  stop('method not implemented yet')
}

#' @keywords internal
#' @export
attribute_skeleton.SpatialPolygonsDataFrame <- function(object, attr.file, ...){
  stop('method not implemented yet')
}

read_attr_file <- function(attr.file){
  # have package default or user setting of delimiter for attr.table?
  if (file.exists(attr.file)){
    read.table(attr.file, sep = ',', stringsAsFactors = FALSE)
  } else {
    NULL
  }
}
