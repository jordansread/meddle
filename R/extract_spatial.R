#' get the bounding box of the feature(s)
#'
#' Calculate the bounding box of the feature(s) in  lat/lon, formatted for metadata field entry
#'
#' @param sp a spatial object from the \code{sp} package
#' @return a list with \code{wbbox}, \code{ebbox}, \code{nbbox}, \code{sbbox} fields
#' @importFrom sp bbox proj4string
#' @examples
#' library(sp)
#' Sr1 <- Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
#' Srs1 <- Polygons(list(Sr1), "s1")
#' Sr2 <- Polygon(cbind(c(-105,-105.5,-106,-105.5,-105),c(31.4,32,34,34,32)))
#' Srs2 <- Polygons(list(Sr2), "s2")
#' p <- SpatialPolygons(list(Srs1, Srs2), proj4string=CRS("+init=epsg:4326 +proj=longlat 
#' +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#' p.df <- SpatialPolygonsDataFrame(Sr = p,
#'    data = data.frame(x=c(0,40), y=c(30,300), z=c(0,0), row.names=c('s1','s2')))
#' \dontrun{
#' library(rgdal)
#' writeOGR(p.df, 'inst/extdata/example_shapefile/','example_shapefile',
#'    driver = 'ESRI Shapefile', overwrite_layer = TRUE)
#' }
#' @export
feature_bbox <- function(sp){
  UseMethod("feature_bbox")
}

#' @keywords internal
#' @export
feature_bbox.Spatial <- function(sp){
  if (!grepl(pattern = 'WGS84', proj4string(sp))){
    stop('sp must be in WGS84 to calculate a valid bounding box')
  }
  bounds <- bbox(sp)
  return(list(wbbox=bounds[1,1], ebbox=bounds[1,2],
              nbbox=bounds[2,2], sbbox=bounds[2,1]))
}

#' @keywords internal
#' @export
feature_bbox.sf <- function(sp){
  feature_bbox(sf::st_geometry(sp))
}

#' @importFrom sf st_crs st_bbox
#' @keywords internal
#' @export
feature_bbox.sfc <- function(sp){
  if (!grepl(pattern = 'WGS84', sf::st_crs(sp)$proj4string)){
    stop('sp must be in WGS84 to calculate a valid bounding box')
  }
  bounds <- sf::st_bbox(sp)
  return(list(wbbox=bounds[['xmin']], ebbox=bounds[['xmax']],
              nbbox=bounds[['ymax']], sbbox=bounds[['ymin']]))
}

feature_type <- function(sp){
  UseMethod("feature_type")
}
#' get the FGDC feature type of spatial object
#'
#' Extract the FGDC feature type from an \code{sp} object
#'
#' @param sp a spatial object from the \code{sp} package 
#' (partial support exists for \code{sf} package for \code{sfc_MULTIPOLYGON} objects)
#' @param return a list with \code{feature-ref} and \code{feature-type} fields
#' @details only classes SpatialPointsDataFrame and SpatialPolygonsDataFrame classes are currently supported
#' @keywords internal
#' @export
feature_type.SpatialPolygons <- function(sp){
  list('feature-ref'="Vector", 'feature-type'="G-polygon")
}
#' @keywords internal
#' @export
feature_type.SpatialPoints <- function(sp){
  list('feature-ref'="Point", 'feature-type'="Point")
}
#' @keywords internal
#' @export
feature_type.sfc_MULTIPOLYGON <- function(sp){
  list('feature-ref'="Vector", 'feature-type'="G-polygon")
}
#' @keywords internal
#' @export
feature_type.sfc_POLYGON <- function(sp){
  list('feature-ref'="Vector", 'feature-type'="G-polygon")
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
#' @keywords internal
#' @export
feature_count.sfc_MULTIPOLYGON <- function(sp){
  list('feature-count'=length(sp))
}
#' @keywords internal
#' @export
feature_count.sfc_POLYGON <- function(sp){
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
#' library(sf)
#' sfc = st_sfc(st_point(c(-89, 45)), st_point(c(-109, 42)), crs = 4326)
#' sf = st_sf(data.frame(val = c('WI_point', 'WY_point'), geom=sfc))
#' feature_states(sf)
feature_states <- function(sf){
  UseMethod("feature_states")
}
#' @keywords internal
#' @export

feature_states.Spatial <- function(sf){
  
  stop('need to convert to sfc')
  
  
  
  return(list(states = feature.states))
}

#' @keywords internal
#' @export
#' @importFrom sf as_Spatial
#' @importFrom dataRetrieval stateCdLookup
feature_states.sfc <- function(sf){
  
  states <- get_states()
  
  state.overlap <- overlaps(sf, states)
  
  as.state_name <- function(x){
    s <- strsplit(tolower(x), " ")
    s_cap <- lapply(s, function(words) {
      ifelse(
        words == 'of',
        words,
        paste0(toupper(substring(words, 1,1)), substring(words, 2))
      )
    })
    sapply(s_cap, paste0, collapse=" ")
  }
  state.names <- unname(sapply(states$ID[state.overlap], as.state_name))
  
  feature.states <- lapply(sort(state.names), function(x){
    list('state-name'=x, 'state-abbr' = dataRetrieval::stateCdLookup(x))
  })
  return(list(states = feature.states))
}
#' @keywords internal
#' @export
#' @importFrom sf st_geometry
feature_states.sf <- function(sf){
  feature_states(st_geometry(sf))
}

#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @importFrom sf st_crs st_as_sf
get_states <- function(){
  us_48 <- sf::st_as_sf(map("state", fill=TRUE, plot=FALSE))
  us_48$names <- paste0("USA:", us_48$ID)
  us_hi <- sf::st_as_sf(map("world", "USA:Hawaii", fill=TRUE, plot=FALSE))
  us_hi$names <- rep("USA:Hawaii", nrow(us_hi))
  us_ak <- sf::st_as_sf(map("world", "USA:Alaska", fill=TRUE, plot=FALSE))
  us_ak$names <- rep("USA:Alaska", nrow(us_ak))
  us_pr <- sf::st_as_sf(map("world2Hires", "Puerto Rico", fill=TRUE, plot=FALSE))
  us_pr$names <- rep("Puerto Rico:Puerto Rico", nrow(us_pr))
  sf::st_geometry(us_pr) <- sf::st_geometry(us_pr) - c(360, 0) # units for PR need a latitude shift
  sf::st_crs(us_pr) <- sf::st_crs(4326)
  
  usa <- rbind(rbind(rbind(us_48, us_ak), us_hi), us_pr)

  return(usa)
}

#' @importFrom sf st_intersects
overlaps <- function(sf0, sf1){
  UseMethod("overlaps")
}

overlaps.SpatialPoints <- function(sp0, sp1){
  overlaps <- gContains(sp1, sp0, byid = TRUE)
  unname(colSums(overlaps) > 0)
}

overlaps.sfc <- function(sf0, sf1){
  unlist(sf::st_intersects(sf0, sf1))
}
overlaps.SpatialPolygons <- function(sp0, sp1){
  unname(!is.na(over(sp1, sp0)))
}

overlaps.SpatialPolygonsDataFrame <- function(sp0, sp1){
  unname(!is.na(over(sp1, sp0)))[, 1]
}

#' extract and summarize spatial data
#'
#' create metadata list from sp object
#'
#' @param x an object of class "Spatial" or a filepath that can be read in
#' @param \dots additional parameters passed to methods.
#' Including \code{out} a character vector of summary values
#' @return a list according to names in spatial lookup tables for tag conversion
#' @export
#' @examples
#' library(sp)
#' Sr1 = Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
#' Srs1 = Polygons(list(Sr1), "s1")
#' p = SpatialPolygons(list(Srs1), proj4string=CRS("+init=epsg:4326 +proj=longlat 
#' +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#' extract_feature(p)
extract_feature <- function(x, ...){
  UseMethod("extract_feature")
}

#' @keywords internal
#' @export
extract_feature.character <- function(x, ...){
  stopifnot(file.exists(x))
  extract_feature(read_data(x), ...)
}

#' @keywords internal
#' @export
extract_feature.Spatial <- function(x, out = c('bbox', 'type', 'count', 'states')){
  feature <- list()
  for (fun in out){
    feature <- append(feature, do.call(paste0('feature_', fun), list(sp=x)))
  }
  return(feature)
}

#' @keywords internal
#' @export
extract_feature.sfc <- function(x, out = c('bbox', 'type', 'count', 'states')){
  feature <- list()
  for (fun in out){
    feature <- append(feature, do.call(paste0('feature_', fun), list(sp=x)))
  }
  return(feature)
}

#' @importFrom sf st_geometry
#' @keywords internal
#' @export
extract_feature.sf <- function(x, out = c('bbox', 'type', 'count', 'states')){
  x <- sf::st_geometry(x)
  extract_feature(x, out = out)
}