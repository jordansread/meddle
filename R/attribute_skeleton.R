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

  attrs <- read_attrs(filename = object)
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

update_attr_table <- function(attrs, attr.file){
  new.attrs <- create_attr_table(attrs)
  old.attrs <- read_attr_file(attr.file)
  attr.table <- merge_attr_table(new.attrs, old.attrs)
  write_attr_file(attr.table, file = attr.file)
}

create_attr_table <- function(attrs){
  empty.ar <- rep(' ', length(attrs))
  data.frame('attr-label'=attrs, 'attr-def' = empty.ar,
             'attr-defs' = empty.ar, 'data-min' = empty.ar,
             'data-max' = empty.ar, 'data-units' = empty.ar,
             stringsAsFactors = FALSE, check.names = FALSE)
}

merge_attr_table <- function(new.attrs, old.attrs){

  if (is.null(old.attrs)){
    return(new.attrs)
  } else {
    stop('need to implement merging code that does the right thing here')
  }
}

#' @export
write_attr_file <- function(x, file){
  ext <- get_filetype(file)
  if (ext == 'csvfile'){
    write.table(x, file, sep=',', row.names = FALSE, quote=TRUE)
  } else if (ext == 'tsvfile'){
    write.table(x, file, sep='\t', row.names = FALSE, quote=TRUE)
  } else stop(file, ' type not supported for write_attr_file', call. = FALSE)

}


#' @export
read_attr_file <- function(attr.file){
  # have package default or user setting of delimiter for attr.table?
  if (file.exists(attr.file)){
    class(attr.file) <- get_filetype(attr.file)
    read_data(attr.file, check.names = FALSE)
  } else {
    NULL
  }
}


#' @importFrom tools file_path_sans_ext
as.attr_file <- function(filename){
  paste0(tools::file_path_sans_ext(filename[1]), '_attributes.csv')
}
