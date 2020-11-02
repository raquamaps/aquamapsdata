library(aquamapsdata)
library(purrr)

am_filter <- function() {

}

#' Get a subset of hcaf record(s)
#'
#' @param am_filter parameters to filter by, for
#' example exclusive economic zone (EEZ),
#' large marine ecosystem (LME), country (CountryMain), etc.
am_hcaf <- function(filter = am_filter()) {

  con <- aquamapsdata::src_sqlite_aquamapsdata()
  on.exit(dbDisconnect(con))

  con %>% tbl("hcaf") %>% filter(am_filter) %>% collect()

# SELECT [ * | fieldname(s) ]
# FROM hcaf
# WHERE fieldname = ‘’
# [ AND | OR ] fieldname = ‘’
# Note:  WHERE fieldname = ‘’ is usually used to get a list of csquarecodes that fall within a certain bounding box. This is later used to get species envelopes (hspen) and map data (hcaf_species_native) of species occurring in that bounding box.
# Example: if user-defined bounding box is: 27N-55S, 20-150E, then the where clause would be:
# WHERE NLimit <= 27
# AND SLimit >= -55
# AND WLimit >= 20
# AND ELimit <= 150

}

am_hcaf()

csquarecodes_within_bb <- function(bb) {

}

am_keys <- function() {
  con <- aquamapsdata::src_sqlite_aquamapsdata()
  on.exit(dbDisconnect(con))
  con %>% tbl("nativemaps") %>% distinct(SpeciesID) %>% collect()
}

am_nativemaps <- function(key) {
  if (! key %in% am_keys())
    stop("Please specify a valid key")
  con <- aquamapsdata::src_sqlite_aquamapsdata()
  on.exit(dbDisconnect(con))

  vals <-
    con %>% tbl("nativemaps") %>%
    filter(SpeciesID == key) %>%
    collect()

  coords <-
    vals %>%
    pull(CsquareCode) %>%
    map_df(csquare_to_dd) %>% collect()

  bind_cols(vals, coords)
}

key <- am_keys()$SpeciesID[1]
nm <- am_nativemaps(key)

am_raster <- function(key) {
  nm <- am_nativemaps(key)
  nr <- diff(range(nm$lat)) / unique(nm$resolution)
  nc <- diff(range(nm$lon)) / unique(nm$resolution)
  ext <- raster::extent(min(nm$lon), max(nm$lon), min(nm$lat), max(nm$lat))
#  ext <- raster::extent(min(nm$lat), max(nm$lat), min(nm$lon), max(nm$lon))
#  r1 <- raster::raster(nrows = nc, ncols = nr, ext = ext, resolution = nm$resolution)
  r1 <- raster::raster(nrows = nr, ncols = nc, ext = ext, resolution = nm$resolution)
  r2 <- raster::rasterize(tibble(nm$lon, nm$lat), r1, nm$probability)
  r2
}

am_raster_leaflet <- function(ras) {

  # r2 <- leaflet::projectRasterForLeaflet(r2, method = "bilinear")

  pal <- leaflet::colorNumeric(
    c("#0C2C84", "#41B6C4", "#FFFFCC"), values(ras),
    na.color = "transparent")

  leaflet() %>% addTiles() %>%
    addRasterImage(ras, colors = pal, opacity = 0.8, project = FALSE) %>%
    addLegend(pal = pal, values = values(ras),
      title = )

}

library(raster)
r2 <- am_raster(key)
values(r2)
image(r2)
plot(r2)

spid <- am_keys()$SpeciesID
con %>% tbl("nativemaps") %>% filter(SpeciesID == spid)
con %>% tbl("taxa") %>% filter(SPECIESID == spid) %>% glimpse()

