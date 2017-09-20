#' SQLite database connection to the aquamaps data using dplyr
#' @importFrom dplyr src_sqlite
#' @importFrom RSQLite dbConnect
aquamapsdata_sqlite <- function() {

  AM_DB <- am_db_path()

  if (!file.exists(AM_DB))
    stop("No database available at ", AM_DB,
         " please use download_db()")

  if (file.size(AM_DB) == 0) {
    message("Removing emtpy db at ", AM_DB)
    unlink(AM_DB)
    stop("Database empty, aborting, please use download_db()")
  }

  my_db <- RSQLite::dbConnect(RSQLite::SQLite(), AM_DB)
  #my_db <- dplyr::src_sqlite(AM_DB)
  return (my_db)
}

am_db_path <- function() {
  paste0(system.file(package = "aquamapsdata"),
         "/extdata/am.db")
}

#' @export
taxa <- function() {
  aquamapsdata_sqlite() %>%
  tbl("taxa")
}

#' @export
nativemaps <- function() {
  aquamapsdata_sqlite() %>%
  tbl("nativemaps")
}

#' @export
hcaf <- function() {
  aquamapsdata_sqlite() %>%
  tbl("hcaf")
}

#' @export
hspen <- function() {
  aquamapsdata_sqlite() %>%
  tbl("hspen")
}

#' @export
occ <- function() {
  aquamapsdata_sqlite() %>%
  tbl("occ")
}
