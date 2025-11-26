.data <- rlang::.data
.env <- rlang::.env

#' @title Utilities
#'
#' @description Basic functions for working with UD rasters
#'
#' @details These might work better in a more generic package.
#'
#' @name Utilities
#'
NULL

#' @param raster_path Path to raster file
#' @param contour_level Contour level to extract (0 - 1)
#' @param crs Coordinate reference system for output polygon
#'
#' @return \code{sf} polygon object
#' @export
#'
#' @describeIn Utilities Extract a contour polygon from a volume UD raster
#'
#' @examples
#'
#' ex_rast <- system.file("ex/elev.tif", package = "terra")
#'
#' ct_90 <- get_contour(ex_rast, 0.9)
#'
#' plot(ct_90)
#'
#' r <- terra::rast(ex_rast)
#'
#' is_vol_ud(r)
#'
#' ud <- get_vol_ud(r)
#'
#' is_vol_ud(ud)
#'
#' terra::plot(ud)
#'
get_contour <- function(
  raster_path,
  contour_level,
  crs = 4326
) {

  # read raster from file
  r <- terra::rast(raster_path)

  # convert to volume UD if needed
  if (!is_vol_ud(r)) {
    r <- get_vol_ud(r)
  }

  r |>
    # set value higher than specified level to NA
    terra::clamp(upper = contour_level, values = FALSE) |>
    # set remaining values to 1
    terra::classify(cbind(-Inf, Inf, 1)) |>
    # convert to polygons
    terra::as.polygons() |>
    # convert to sf
    sf::st_as_sf() |>
    # set crs
    sf::st_transform(crs)

}

#' @param r Raster object
#'
#' @return Raster object
#' @export
#'
#' @describeIn Utilities Convert a raster to a volume UD in which each cell
#'   contains the CDF value corresponding to the value from the original raster
#'
get_vol_ud <- function(r) {

  # get raster values
  v <- terra::values(r)

  # sum for calculating UD
  s <- sum(v, na.rm = TRUE)

  # ranks for sorting values
  ranks <- rank(v, ties.method = "first")

  # calculate cumulative distribution values
  vol_ud <- (s - cumsum(sort(v))[ranks]) / s

  # ensure values fall between 0 (for highest input value) and 1 (for lowest)
  vol_ud[vol_ud < 0] <- 0
  vol_ud[vol_ud > 1 | is.na(vol_ud)] <- 1

  # return cdf values as raster
  terra::values(r) <- vol_ud
  r

}

#' @param r Raster object
#'
#' @return logical
#' @export
#'
#' @describeIn Utilities Check the values of a raster to see if it has already
#'   been converted to a volume UD. Simply checks if values range from 0 to 1.
#'
is_vol_ud <- function(r) {
  # get raster values
  v <- terra::values(r)
  # chack that all values are between 0 and 1
  identical(range(v, na.rm = TRUE), c(0, 1))
}
