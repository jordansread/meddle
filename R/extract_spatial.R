
feature_bbox <- function(sp){
  UseMethod("feature_bbox")
}
#' get the bounding box of the feature(s)
#'
#' Calculate the bounding box of the feature(s) in  lat/lon, formatted for metadata field entry
#'
#' @param sp a spatial object from the \code{sp} package
#' @return a list with \code{wbbox}, \code{ebbox}, \code{nbbox}, \code{sbbox} fields
#' @importFrom sp bbox proj4string
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

#' get the states that features overlap with
#'
#' summarize the spatial extent of data relative to overlap w/ US states
#'
#' @param sp an object of class \code{Spatial}
#' @return a list with \code{states} field
#' @export
#' @examples
#' library(sp)
#' p = SpatialPoints(cbind(-89,42), proj4string=CRS("+proj=longlat +datum=WGS84"))
#' feature_states(p)
#'
#' Sr1 = Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
#' Srs1 = Polygons(list(Sr1), "s1")
#' p = SpatialPolygons(list(Srs1), proj4string=CRS("+proj=longlat +datum=WGS84"))
#' feature_states(p)
feature_states <- function(sp){
  UseMethod("feature_states")
}
#' @keywords internal
#' @export
#' @importFrom dataRetrieval stateCdLookup
feature_states.Spatial <- function(sp){
  states <- get_states()
  state.overlap <- overlaps(sp, states)
  as.state_name <- function(x){
    s <- strsplit(x, " ")[[1]]
    paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse=" ")
  }
  state.names <- unname(sapply(names(states)[state.overlap], as.state_name))
  feature.states <- lapply(sort(state.names), function(x) list('state-name'=x, 'state-abbr' = dataRetrieval::stateCdLookup(x)))
  return(list(states = feature.states))
}


#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @importFrom sp CRS
get_states <- function(){
  us_48 <- map("state", fill=TRUE, plot=FALSE)
  us_hi <- map("world", c("USA:Hawaii"), fill=TRUE, plot=FALSE)
  us_ak <- map("world", c("USA:Alaska"), fill=TRUE, plot=FALSE)
  usa <- c(us_48, us_hi, us_ak)

  usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  return(usa)
}

#' @importFrom rgeos gOverlaps gContains gSimplify
overlaps <- function(sp0, sp1){
  UseMethod("overlaps")
}

overlaps.SpatialPoints <- function(sp0, sp1){
  overlaps <- gContains(sp1, sp0, byid = TRUE)
  unname(colSums(overlaps) > 0)
}

overlaps.SpatialPolygons <- function(sp0, sp1){
  overlaps <- gOverlaps(sp1, gSimplify(sp0, tol=0.001), byid = TRUE)
  unname(colSums(overlaps) > 0)
}

#' extract and summarize spatial data
#'
#' create metadata list from sp object
#'
#' @param sp an object of class "Spatial"
#' @param out a character vector of summary values
#' @return a list according to names in spatial lookup tables for tag conversion
#' @export
#' @examples
#' library(sp)
#' Sr1 = Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
#' Srs1 = Polygons(list(Sr1), "s1")
#' p = SpatialPolygons(list(Srs1), proj4string=CRS("+proj=longlat +datum=WGS84"))
#' extract_feature(p)
extract_feature <- function(sp, out = c('bbox', 'type', 'count', 'states')){
  feature <- list()
  for (fun in out){
    feature <- append(feature, do.call(paste0('feature_', fun), list(sp=sp)))
  }
  return(feature)
}
