
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

#' merge two attribute tables
#'
#' merge two attribute tables, assuming that the latter one
#' has more complete data, but the newer one has all of the applicable fields.
#' Fields in \code{old.attrs} that don't appear in \code{new.attrs} will be dropped.
#' Likewise, fields in \code{new.attrs} but not in \code{old.attrs} will retain
#' the values in \code{new.attrs}.
#'
#' @param new.attrs an attribute table (data.frame)
#' @param old.attrs an attribute table (data.frame)
#' @return a merged attribute table.
merge_attr_table <- function(new.attrs, old.attrs){

  # to do: check that these two share the same headers and number of columns
  if (!is.null(old.attrs)){
    old.labels <- old.attrs$`attr-label`
    for (j in 1:nrow(new.attrs)){
      attr.label <- new.attrs$`attr-label`[j]
      if (attr.label %in% old.labels){
        replc.i <- which(old.labels == attr.label)[1]
        new.attrs[j, ] <- old.attrs[replc.i, ]
      }
    }
  }
  return(new.attrs)
}

#' write a file formatted as attributes
#'
#' write external file for attributes
#'
#' @param x the data to be written to file
#' @param file a filepath to a file that is comma or tab delimited
#' @rdname attr_file
#' @export
write_attr_file <- function(x, file){
  ext <- get_filetype(file)
  if (ext == 'csvfile'){
    write.table(x, file, sep=',', row.names = FALSE, quote=FALSE)
  } else if (ext == 'tsvfile'){
    write.table(x, file, sep='\t', row.names = FALSE, quote=FALSE)
  } else stop(file, ' type not supported for write_attr_file', call. = FALSE)

}

#' read in a file formatted as attributes
#'
#' read in external file for attributes
#'
#' @param attr.file a filepath to a file that is comma or tab delimited
#' @return the data.frame of formatted attributes
#' @rdname attr_file
#' @export
read_attr_file <- function(attr.file){
  # have package default or user setting of delimiter for attr.table?
  if (file.exists(attr.file)){
    class(attr.file) <- get_filetype(attr.file)
    read_data(attr.file, check.names = FALSE, na.strings = ' ')
  } else {
    NULL
  }
}


#' read attributes from a recognized file type
#'
#' return just the header names (attributes) for delimited files or shapefiles
#'
#' @param filename the full path for the file(s)
#' @return the attributes as a character vector
#' @export
get_attrs <- function(filename){
  class(filename) <- get_filetype(filename)
  UseMethod("get_attrs", object = filename)
}

#' @export
#' @keywords internal
get_attrs.csvfile <- function(filename){
  strsplit(readLines(filename, n = 1L), '[,]')[[1]]
}

#' @export
#' @keywords internal
get_attrs.tsvfile <- function(filename){
  strsplit(readLines(filename, n = 1L), '[\t]')[[1]]
}

#' @export
#' @keywords internal
get_attrs.shapedir <- function(filename){
  filename <- file.path(filename, dir(filename))
  get_attrs.shapefile(filename)
}

#' get attributes from a shapefile
#'
#' extract the attribute names from a shapefile file collection.
#'
#'
#' @param filename a character vector of shapefile file names
#' @return a character vector of attribute names for use in metadata rendering
#'
#' @details
#' Vector of shapefile file names (the \code{filename} arguments) must include a *.dbf file.
#' This function only reads the shapefile's dbf, so it skips reading in the geometry.
#' @importFrom foreign read.dbf
#' @export
#' @keywords internal
get_attrs.shapefile <- function(filename){
  dbf.file <- filename[grepl(pattern = '.dbf', x = filename)]
  data <- foreign::read.dbf(dbf.file)
  return(names(data))
}

#' create name for attributes file if one doesn't exist
#'
#' create attribute file name
#'
#' @param filename the name of the files that this attribute file will be based on
#' @return a filepath for an attributes file
#'
#' @importFrom tools file_path_sans_ext
as.attr_file <- function(filename){
  paste0(tools::file_path_sans_ext(filename[1]), '_attributes.csv')
}



#' read and format attributes from delimited file or data.frame
#'
#' formats an attribute table into a list appropriate for metadata rendering
#'
#' @param x a file name for attributes file or a data.frame from \code{\link{read_attr_file}}
#' @return a list of attributes and values for rendering
#' @keywords internal
#' @importFrom methods is
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

