
#' get the bounding box of the feature(s)
#'
#' Calculate the bounding box of the feature(s) in  lat/lon, formatted for metadata field entry
#'
#' @param sp a spatial object from the \code{sp} package
#' @return a list with \code{wbbox}, \code{ebbox}, \code{nbbox}, \code{sbbox} fields
#' @importFrom sp bbox
#' @keywords internal
#' @export
get_bbox <- function(sp){
  if (!grepl(pattern = 'WGS84', proj4string(sp))){
    stop('sp must be in WGS84 to calculate a valid bounding box')
  }
  bounds <- bbox(sp)
  return(list(wbbox=bounds[1,1], ebbox=bounds[1,2],
              nbbox=bounds[2,1], sbbox=bounds[2,2]))
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
get_feature_type <- function(sp){
  feature.type = switch(class(sp),
                        "SpatialPointsDataFrame" = "Point",
                        "SpatialPolygonsDataFrame" = "G-polygon")
  list('feature-type'=feature.type)
}

#' get the feature count from a spatial object
#'
#' Tally the number of features in a \code{sp} object
#'
#' @param sp a spatial object from the \code{sp} package
#' @param return a list with \code{feature-count} field
#' @keywords internal
#' @export
get_feature_count <- function(sp){
  list('feature-count'=length(sp))
}

