#' create a skeleton file (or safely append to existing) for attributes
#'
#' create skeleton from object
#'
#' @param object a regognized R object or vector of file paths that can be read in to a recognized object class
#' @param attr.file a file to write the attribute skeleton to
#' @param \dots additional parameters passed to methods
#' @details
#' if the attribute file (\code{attr.file}) doesn't exist, a skeleton formatted file is created.
#' If the file does exist, it is opened and any new fields are joined in. Existing content in
#' \code{attr.file} is not overwritten.
#' @export
attribute_skeleton <- function(object, attr.file, ...){
  UseMethod("attribute_skeleton")
}

#' @keywords internal
#' @export
attribute_skeleton.data.frame <- function(object, attr.file, ...){
  attrs <- names(object)
  update_attr_table(attrs, attr.file)
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
  class(object) <- get_filetype(object)

  attrs <- get_attrs(filename = object)
  update_attr_table(attrs, attr.file)
}

#' @keywords internal
#' @export
attribute_skeleton.SpatialPointsDataFrame <- function(object, attr.file, ...){
  attrs <- names(object)
  update_attr_table(attrs, attr.file)
}

#' @keywords internal
#' @export
attribute_skeleton.SpatialPolygonsDataFrame <- function(object, attr.file, ...){
  attrs <- names(object)
  update_attr_table(attrs, attr.file)
}

