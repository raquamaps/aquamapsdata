#' Raster with native habitat map data for one or several species
#' @details if the fun parameter is omitted, the probability field will be
#'   used
#' @param key a string identifier for the species
#' @param resolution the grid resolution in degrees, by default 0.5
#' @param fun the aggregation fun to use when grid cells are
#'   covered by multiple spatial features, by default their count
#' @importFrom raster raster rasterize extent
#' @importFrom sp SpatialPoints proj4string CRS
#' @importFrom stats na.omit
#' @examples \dontrun{
#' ras <- am_raster(am_search_fuzzy("trevally")$key)
#' }
#' @export
#' @family spatial
am_raster <- function(key, resolution = 0.5,
                      fun = c("count", "last", "first")) {

  nm <- am_nativemaps(key)
  nm$resolution <- resolution
  nr <- diff(range(nm$lat)) / unique(nm$resolution)
  nc <- diff(range(nm$lon)) / unique(nm$resolution)
  ext <- raster::extent(min(nm$lon), max(nm$lon), min(nm$lat), max(nm$lat))

  o <-
    sp::SpatialPoints(coords = na.omit(data.frame(nm$lon, nm$lat),
      sp::proj4string(CRS("+init=epsg:4326"))))

  r1 <-
    raster::raster(nrows = nr, ncols = nc, ext = ext,
      resolution = nm$resolution)

  if (missing(fun))
    return(raster::rasterize(o, r1, field = nm$Probability))

  raster::rasterize(o, r1, crs = CRS("+init=epsg:4326"), fun = fun)
}

#' Map for native habitat
#'
#' Shows a leaflet map for the native habitat given a species identifier
#' @param ras a raster object, for example from the am_raster function
#' @param title a string to use as legend title, by default blank
#' @param cols a vector with three hex colors to use for a numeric color
#' legend, default: c("#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")
#' @examples \dontrun{
#' # native habitat for blue trevally
#' ras <- am_raster(am_search_fuzzy("blue trevally")$key)
#' am_map_leaflet(ras, title = "Blue trevally (p)")
#'
#' # where do both blue and white trevally occur?
#' ras <- am_raster(am_search_fuzzy("trevally AND (white OR blue)")$key,
#'   fun = "count")
#' am_map_leaflet(ras, title = "Blue and white trevally (n)")
#' }
#' @export
#' @importFrom leaflet leaflet addTiles addRasterImage addLegend colorNumeric projectRasterForLeaflet
#' @importFrom raster values
#' @family spatial
am_map_leaflet <- function(ras, title = "",
  cols = c("#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")) {

  # RColorBrewer::brewer.pal(7, "YlOrRd")[3:7]

  ras <- leaflet::projectRasterForLeaflet(ras, method = "bilinear")

  #pal <- leaflet::colorNumeric(cols,
  #  raster::values(ras),
  #  na.color = "transparent")
  #pal <- colorFactor(colors, levels = breaks, ordered = TRUE, na.color = "transparent")

  pal <- leaflet::colorBin(cols, na.omit(unique(values(ras))),
    bins = length(cols), pretty = TRUE, na.color = "#00000000")

  e <- raster::extent(ras)

  leaflet() %>%
    leaflet::addProviderTiles(provider = "Esri.OceanBasemap") %>%
    leaflet::addRasterImage(ras, project = FALSE,
      colors = pal, opacity = 0.8) %>%
    addLegend(values = raster::values(ras),
      title = title, pal = pal) %>%
    leaflet::fitBounds(lng1 = e@xmin, lat1 = e@ymin, lng2 = e@xmax, lat2 = e@ymax)

}

#' Converts location coordinate data into raster data
#' @param lon decimal longitude for coordinates
#' @param lat decimal latitude for coordinates
#' @param fun aggregation method when several points occur in one cell
#' @return a raster grid with individual cell values representing
#' the count of occurrences within the grid cells using WGS84
#' @family spatial
rasterize_coords <- function(lon, lat, fun = "count") {
  o <- sp::SpatialPoints(coords = na.omit(data.frame(lon, lat),
    sp::proj4string(CRS("+init=epsg:4326"))))

  raster::rasterize(o,
    raster(ncol = 720, nrow = 360, crs = CRS("+init=epsg:4326")),
    fun = fun)
}

#' Raster grid cell identifiers (LOICZID) with presence
#' @param r raster (needs to be half degree cell raster WGS84 720x360)
#' @param lower_limit raster cell values higher than this parameter
#' is interpreted as presence
#' @return a vector of raster grid cell identifiers (loiczids)
#' @family spatial
which_cells_in_raster <- function(r, lower_limit = 0) {
  loiczids <- which(values(r) > lower_limit)
  return (loiczids)
}

#' Cell identifiers for an extent or bounding box
#'
#' Half degree grid cell identifiers (CsquareCode strings) within an
#' extent or bounding box.
#' @param x1 decimal degree WGS84 longitude value for extent (xmin)
#' @param x2 decimal degree WGS84 longitude value for extent (xmax)
#' @param y1 decimal degree WGS84 latitude value for extent (ymin)
#' @param y2 decimal degree WGS84 latitude value for extent (ymax)
#' @return tibble with CsquareCode identifiers
#' @examples \dontrun{
#' am_csc_from_extent(-5, 5, -7, -5)
#' }
#' @export
#' @importFrom rlang .data
#' @family spatial
am_csc_from_extent <- function(x1, x2, y1, y2) {

  stopifnot(x2 >= x1 & y2 >= y1)

  am_hcaf() %>%
    filter(
      .data$CenterLong >= x1, .data$CenterLong <= x2,
      .data$CenterLat >= y1, .data$CenterLat <= y2) %>%
    select(.data$CsquareCode) %>%
    collect()
}

#' Species occuring in set of grid cell
#'
#' Given a set of grid cell identifiers or CsquareCodes, this function
#' returns a list of the species that occur there
#' @param csc a vector of strings with CsquareCodes (grid cell identifiers)
#' @param min_prob a numeric with the minimum threshold for probable occurrence,
#'   default 0
#' @examples \dontrun{
#' # distinct number of species in a specific grid cell
#' am_species_in_csc("7516:236:1", 0.99)
#'
#' # other species likely present in the same area as the bluespotted trevally
#' xt <- am_raster(am_search_fuzzy("bluespotted trevally")$key)@extent
#' csc <- am_csc_from_extent(xt@xmin, xt@xmax, xt@ymin, xt@ymax)$CsquareCode
#' am_species_in_csc(csc, min_prob = 0.9)
#' }
#' @importFrom rlang .data
#' @export
#' @family spatial
am_species_in_csc <- function(csc, min_prob = 0) {

  db_cache$local_db %>%
    dplyr::tbl("hcaf_species_native") %>%
    dplyr::filter(.data$CsquareCode %in% csc) %>%
    dplyr::filter(.data$Probability >= min_prob) %>%
    dplyr::group_by(.data$SpeciesID) %>%
    dplyr::count(.data$SpeciesID) %>%
    dplyr::collect() %>%
    dplyr::arrange(desc(n))

}

#' Total count for number of distinct species across cells, ie "richness"
#'
#' Global richness map data (i.e. number of different species per cell),
#' optionally filtered for taxonomy and a preferred probability threshold
#' @param csc a vector of strings with CsquareCodes (grid cell identifiers)
#' @param min_prob a numeric with the threshold for probable occurrence,
#'   default 0
#' @param keys a vector of string with SpeciesID values
#' @examples \dontrun{
#' # distinct number of species in a specific grid cell
#' am_species_per_csc("7516:236:1", 0.99)
#'
#' # distinct species count per grid cells in LME 1
#' csc <- am_hcaf() %>% filter(LME == 1) %>% collect() %>% pull(CsquareCode)
#' am_species_per_csc(csc, 0.9)
#'
#' # global richness map data based on taxonomy
#' keys <- am_search_exact(Family = "Carangidae")$SpeciesID
#' am_species_per_csc(keys = keys, min_prob = 0.9)
#' }
#' @importFrom rlang .data
#' @export
#' @family spatial
am_species_per_csc <- function(csc, min_prob = 0, keys = NULL) {

  t1 <-
    db_cache$local_db %>%
    dplyr::tbl("hcaf_species_native")

  if (!missing(csc) && !is.null(csc))
    t2 <- t1 %>% dplyr::filter(.data$CsquareCode %in% csc)

  if (!missing(keys) && !is.null(keys))
    t2 <- t1 %>% dplyr::filter(.data$SpeciesID %in% keys)

  t2 %>%
    dplyr::filter(.data$Probability >= min_prob) %>%
    dplyr::group_by(.data$CsquareCode) %>%
    dplyr::summarise(n_species = n_distinct(.data$SpeciesID)) %>%
    dplyr::collect() %>%
    dplyr::arrange(desc(.data$n_species))

}

