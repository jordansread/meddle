
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
    if (dir.exists(filename) && 'shp' %in% tools::file_ext(dir(filename))){
      type <- 'shapedir'
    } else {
      type <- paste0(tools::file_ext(filename),'file')
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

