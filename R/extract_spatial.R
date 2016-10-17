
feature_bbox <- function(sp){
  UseMethod("feature_bbox")
}
#' get the bounding box of the feature(s)
#'
#' Calculate the bounding box of the feature(s) in  lat/lon, formatted for metadata field entry
#'
#' @param sp a spatial object from the \code{sp} package
#' @return a list with \code{wbbox}, \code{ebbox}, \code{nbbox}, \code{sbbox} fields
#' @importFrom sp bbox
#' @keywords internal
#' @export
feature_bbox.Spatial <- function(sp){
  if (!grepl(pattern = 'WGS84', proj4string(sp))){
    stop('sp must be in WGS84 to calculate a valid bounding box')
  }
  bounds <- bbox(sp)
  return(list(wbbox=bounds[1,1], ebbox=bounds[1,2],
              nbbox=bounds[2,1], sbbox=bounds[2,2]))
}


feature_type <- function(sp){
  UseMethod("feature_type")
}
#' get the FGDC feature type of spatial object
#'
#' Extract the FGDC feature type from an \code{sp} object
#'
#' @param sp a spatial object from the \code{sp} package
#' @param return a list with \code{feature-type} field
#' @details only classes SpatialPointsDataFrame and SpatialPolygonsDataFrame classes are currently supported
#' @keywords internal
#' @export
feature_type.SpatialPolygons <- function(sp){
  list('feature-type'="G-polygon")
}
#' @keywords internal
#' @export
feature_type.SpatialPoints <- function(sp){
  list('feature-type'="Point")
}

feature_count <- function(sp){
  UseMethod("feature_count")
}
#' get the feature count from a spatial object
#'
#' Tally the number of features in a \code{sp} object
#'
#' @param sp a spatial object from the \code{sp} package
#' @param return a list with \code{feature-count} field
#' @keywords internal
#' @export
feature_count.Spatial <- function(sp){
  list('feature-count'=length(sp))
}

#' extract and summarize spatial data
#'
#' create metadata list from sp object
#'
#' @param sp an object of class "Spatial"
#' @param out a character vector of summary values
#' @return a list according to names in spatial lookup tables for tag conversion
#' @export
extract_feature <- function(sp, out = c('bbox', 'type', 'count')){
  feature <- list()
  for (fun in out){
    feature <- append(feature, do.call(paste0('feature_', fun), list(sp=sp)))
  }
  return(feature)
}
