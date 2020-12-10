#' Perform downloading of compressed SQLite db, storing it locally
#' @param force boolean to indicate whether to overwrite an existing db
#' @param passphrase passphrase if using encrypted db, default: NULL
#' @examples \dontrun{
#' download_db()
#' }
#' @importFrom R.utils gunzip isGzipped
#' @importFrom utils download.file
#' @importFrom rcrypt encrypt decrypt
#' @export
#' @family general
download_db <- function(force = FALSE, passphrase = NULL) {

  src <- "https://archive.org/download/aquamapsdb/am.db.gpg"
  #"http://archive.org/download/aquamapsdata/am.db.gpg"
  temp <- file.path(dirname(tempdir()), "am.db.gpg")
  tgt <- am_db_sqlite()

  if (file.exists(tgt) && !force)
    stop("An existing db exists at ", tgt,
         ", to overwrite, pls rerun with force = TRUE")

  message("Download of aquamapsdb (",
            src, " -> ", temp, " -> ", tgt, ")")

  if (!file.exists(temp)) {
    download.file(src, temp)
  } else {
    message("data appears to have been downloaded already?")
    message("proceeding to extract data ....")
  }

  if (!file.exists(tgt)) {
    message("... decrypting ", temp, " to ", tgt)
    rcrypt::decrypt(temp, passphrase = passphrase, output = tgt)
    message("done")
  } else {
    message("data appears to have been extracted already?")
  }
  return(invisible(file.exists(tgt)))
}
