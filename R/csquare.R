#' csquare code conversion from decimal degrees
#'
#' The csquare code is explained here:
#' http://www.cmar.csiro.au/csquares/csq-faq.htm
#'
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' @param resolution the resolution in decimal degress
#' (one of 0.1, 0.01, 0.001, 0.0001 etc)
#' @return a character string with the code
#' @importFrom purrr map2_dbl
#' @examples
#' csquare_from_dd(50.93578, -114.01435, resolution = 0.00001)
#' @export
#' @family spatial
csquare_from_dd <- function(lat, lon, resolution = 0.0001) {

  scale <- 1 / resolution
  n_dec <- log10(scale)

  lat_i <- scale * lat
  lon_i <- scale * lon

  whole_lat <- sprintf("%02d", floor(abs(lat)))
  whole_lon <- sprintf("%03d", floor(abs(lon)))

  decimal_split <- function(x, n_digits) {
    int_decimals <- as.double((abs(x) - floor(abs(x))) * 10^n_digits)
    res <- as.integer(unlist(strsplit(
      split = "",
      sprintf(paste0("%0", n_digits, "g"), int_decimals)
    )))
  }

  fractional_lat <- decimal_split(lat, n_dec)
  fractional_lon <- decimal_split(lon, n_dec)

  quartet_beg <- function(lat, lon) {
    if (lat >= 0) {
      return(ifelse(lon >= 0, 1, 7))
    }
    return(ifelse(lon >= 0, 3, 5))
  }

  triplet_beg <- function(lat, lon) {
    if (lat <= 4 && lon <= 4) {
      return(1)
    }
    if (lat <= 4 && lon > 4) {
      return(2)
    }
    if (lat > 4 && lon <= 4) {
      return(3)
    }
    if (lat > 4 && lon > 4) {
      return(4)
    }
    warning("triplet_beg called with lat: ", lat, " lon: ", lon)
    return(NA)
  }

  pos1 <-
    purrr::map2_dbl(fractional_lat, fractional_lon,
      function(x, y) triplet_beg(x, y))

  pos2 <- fractional_lat
  pos3 <- fractional_lon

  fractional_triplets <- paste0(pos1, pos2, pos3)

  quartet_lat <- gsub("(\\d+)(\\d{1})", "\\1", whole_lat)
  quartet_lon <- gsub("(\\d+)(\\d{1})", "\\1", whole_lon)

  quartet_head <- quartet_beg(
    as.integer(quartet_lat) * sign(lat),
    as.integer(quartet_lon) * sign(lon))

  quartet <- paste0(quartet_head, quartet_lat, quartet_lon)

  triplet_whole_lat <- gsub("(\\d+)(\\d{1})", "\\2", whole_lat)
  triplet_whole_lon <- gsub("(\\d+)(\\d{1})", "\\2", whole_lon)
  triplet_head <- triplet_beg(triplet_whole_lat, triplet_whole_lon)

  triplet_one <- paste0(triplet_head, triplet_whole_lat,
    triplet_whole_lon)

  triplets <- paste(collapse = ":",
    c(quartet, triplet_one, fractional_triplets))

  return(triplets)
}


#' csquare code conversion to decimal degrees
#'
#' The csquare code is explained here:
#' http://www.cmar.csiro.au/csquares/csq-faq.htm
#'
#' @param code csquare code
#' @return a list with decimal degrees lat and lon and resolution
#' @examples \dontrun{
#' csquare_to_dd("7307:487:380:383")
#' }
#' @importFrom stringi stri_match_all_regex
#' @export
#' @family spatial

csquare_to_dd <- function(code) {
  re <- "(\\d{1})(\\d{1})(\\d{2}):((\\d{3}):)*(\\d{1}$)*"

  prefix <-
    as.numeric(stri_match_all_regex(code, re)[[1]][, 2])

  pl <- c("NE" = 1, "SE" = 3, "SW" = 5, "NW" = 7)
  NSWE <- names(which(pl == prefix))

  # second digits in triplets
  digit_lat_1 <-
    as.numeric(stri_match_all_regex(code, re)[[1]][, 3])

  # third digits in triplets
  digit_lon_12 <-
    as.numeric(stri_match_all_regex(code, re)[[1]][, 4])

  digit_triplets <-
    stri_match_all_regex(code, ":(\\d{3})")[[1]][, 2]

  triplets <- strsplit(digit_triplets, "")
  pos1 <- sapply(triplets, "[[", 1)
  pos2 <- sapply(triplets, "[[", 2)
  pos3 <- sapply(triplets, "[[", 3)

  lat <- paste0(
    collapse = "",
    c(digit_lat_1, pos2[1], ".", pos2[-1])
  )

  lon <- paste0(
    collapse = "",
    c(digit_lon_12, pos3[1], ".", pos3[-1])
  )

  resolution <- 10^-length(triplets)

  digit_tail <-
    as.integer(stri_match_all_regex(code, re)[[1]][, 7])

  if (!is.na(digit_tail)) {
    resolution <- resolution * 5

    if (digit_tail == 3 || digit_tail == 4) {
      lat <- paste0(lat, "5")
    }

    if (digit_tail == 2 || digit_tail == 4) {
      lon <- paste0(lon, "5")
    }
  }

  res <- list(
    initial = NSWE,
    lat = as.double(lat),
    lon = as.double(lon),
    resolution = resolution
  )

  return(res)
}

# There are two "special cases" where the above treatment needs
# additional explanation and/or a slight adjustment:
# (i) values of zero latitude or longitude are treated as positive,
# i.e. 0 latitude is always in the NE or NW global quadrant (not SE or SW),
# 0 longitude in the NE or SE quadrant (not NW or SW).
# (ii) values on the "edge of the map", i.e. latitude +90 or -90,
# longitude +180 or -180, are treated as (e.g.) +89.9999... , +179.9999...,
# because otherwise they would end up being assigned to 10 x 10 degree squares
# which don't exist (since, for example, there isn't a square extending
# from 90 N to 100 N, or 180 E to 190 E).
