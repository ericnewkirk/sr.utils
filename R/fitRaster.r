#' fitRaster
#'
#' Zoom \code{leaflet} map to raster extent
#'
#' @param lf \code{leaflet} object or proxy
#' @param r \code{terra} raster or filepath
#'
#' @return \code{leaflet} object
#' @export
#'
#' @examples
#'
#' r <- "M:/SeasonalRangeUpdates/Output/BS516/ctmm_YRL_avg_Jan01_Dec31.tif"
#'
#' leaflet::leaflet() |>
#'   leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) |>
#'   fitRaster(r)
#'
fitRaster <- function(lf, r) {

  if (!inherits(r, "SpatRaster")) {
    r <- terra::rast(r)
  }

  bounds <- r |>
    terra::as.polygons(extent = TRUE) |>
    sf::st_as_sf() |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    as.character()
  
  leaflet::fitBounds(lf, bounds[1], bounds[2], bounds[3], bounds[4])

}
