#' read data from supported file formats
#'
#' read in data from filepath into R
#'
#' @param filename a complete file path
#' @param \dots additional arguments passed to file-specific methods.
#' @export
read_data <- function(filename, ...){
  UseMethod("read_data")
}

#' @export
#' @keywords internal
read_data.character <- function(filename, ...){
  stopifnot(file.exists(filename))
  class(filename) <- get_filetype(filename)
  UseMethod("read_data", object = filename)
}

#' @importFrom rgdal readOGR
#' @importFrom tools file_path_sans_ext
#' @importFrom utils read.table write.table
#' @export
#' @keywords internal
read_data.shapefile <- function(filename, ...){
  rgdal::readOGR(dirname(filename[1]), layer = basename(tools::file_path_sans_ext(filename[1])), ...)
}

#' @export
#' @keywords internal
read_data.shapedir <- function(filename, ...){
  rgdal::readOGR(filename, layer = tools::file_path_sans_ext(dir(filename)[1]), ...)
}

#' @export
#' @keywords internal
read_data.gzfile <- function(filename, ...){
  file.con <- gzfile(filename)
  if (grepl('tsv', filename)){
    read_data.tsvfile(file.con, ...)
  } else if (grepl('csv', filename)){
    read_data.csvfile(file.con, ...)
  } else {
    stop('type ', filename, ' not recognized by read_data', call. = FALSE)
  }
  close(file.con)
}
#' @export
#' @keywords internal
read_data.csvfile <- function(filename, ...){
  read.table(filename, header=TRUE, sep=',', stringsAsFactors = FALSE, ...)
}

#' @export
#' @keywords internal
read_data.tsvfile <- function(filename, ...){
  read.table(filename, header=TRUE, sep='\t', stringsAsFactors = FALSE, ...)
}


#' @importFrom tools file_ext
get_filetype <- function(filename){
  if (length(filename) == 1){
    if (dir.exists(filename) && 'shp' %in% tools::file_ext(dir(filename))){
      type <- 'shapedir'
    } else {
      type <- paste0(tools::file_ext(filename), 'file')
    }

  } else {
    if (length(filename) > 1 && 'shp' %in% tools::file_ext(filename)){
      type <- 'shapefile'
    } else {
      stop(paste(filename, collapse=', '), ' unrecognized', call. = FALSE)
    }
  }
  return(type)
}

