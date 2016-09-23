
read_data <- function(filename, ...){
  UseMethod("read_data")
}

#' @importFrom rgdal readOGR
#' @importFrom tools file_path_sans_ext
read_data.shapefile <- function(filename, ...){
  rgdal::readOGR(dirname(filename[1]), layer = tools::file_path_sans_ext(filename[1]), ...)
}

read_data.csvfile <- function(filename, ...){
  read.table(filename, header=TRUE, sep=',', stringsAsFactors = FALSE, ...)
}

read_data.tsvfile <- function(filename, ...){
  read.table(filename, header=TRUE, sep='\t', stringsAsFactors = FALSE, ...)
}


#' @importFrom tools file_ext
get_filetype <- function(filename){
  if (length(filename) == 1){
    type <- paste0(tools::file_ext(filename),'file')
  } else {
    if (length(filename) > 1 && 'shp' %in% tools::file_ext(filename)){
      type <- 'shapefile'
    } else {
      stop(paste(filename, collapse=', '), ' unrecognized', call. = FALSE)
    }
  }
  return(type)
}

#' read attributes from a recognized file
#'
#' return just the header names (attributes) for delimited files or shapefiles
#'
#' @param filename the full path for the file(s)
#' @return the attributes as a character vector
#' @export
read_attrs <- function(filename){
  class(filename) <- get_filetype(filename)
  UseMethod("read_attrs", object = filename)
}

read_attrs.csvfile <- function(filename){
  strsplit(readLines(filename, n = 1L), '[,]')[[1]]
}

read_attrs.tsvfile <- function(filename){
  strsplit(readLines(filename, n = 1L), '[\t]')[[1]]
}

#' @importFrom foreign read.dbf
#' @export
#' @keywords internal
read_attrs.shapefile <- function(filename){
  dbf.file <- filename[grepl(pattern = '.dbf', x = filename)]
  data <- foreign::read.dbf(dbf.file)
  return(names(data))
}
