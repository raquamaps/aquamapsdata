#' Perform downloading of compressed SQLite db, storing it locally
#' @param force boolean to indicate whether to overwrite an existing db
#' @examples \dontrun{
#' download_db()
#' }
#' @importFrom R.utils gunzip isGzipped bunzip2
#' @importFrom curl curl_download
#' @export
#' @family general
download_db <- function(force = FALSE) {

  src <- "https://archive.org/download/aquamapsdb/am.db.bz2"
  temp <- file.path(dirname(tempdir()), "am.db.bz2")
  tgt <- am_db_sqlite()

  if (file.exists(tgt) && !force)
    stop("An existing db exists at ", tgt,
      ", to overwrite, pls rerun with force = TRUE")

  message("Download of aquamapsdb (", src, " -> ", temp, " -> ", tgt, ")")
  if (!file.exists(temp)) {
    dl <- curl::curl_download(src, temp, quiet = FALSE)
  } else {
    message("data appears to have been downloaded already?")
    message("proceeding to extract data ....")
  }

  if (!file.exists(tgt)) {
    if (!dir.exists(dirname(tgt))) dir.create(dirname(tgt), recursive = TRUE)
    message("... unpacking ", temp, " to ", tgt)
    R.utils::bunzip2(filename = temp, destname = tgt)
    message("done")
  } else {
    message("data appears to have been extracted already?")
  }
  return(invisible(file.exists(tgt)))
}
