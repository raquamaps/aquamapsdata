#' Perform downloading of compressed SQLite db, storing it locally
#' @param force boolean to indicate whether to overwrite an existing db
#' @param passphrase passphrase if using encrypted db, default: NULL
#' @examples \dontrun{
#' download_db()
#' }
#' @importFrom R.utils gunzip isGzipped bunzip2
#' @importFrom curl curl_download
#' @importFrom rcrypt encrypt decrypt
#' @export
#' @family general
download_db <- function(force = FALSE, passphrase = NULL) {

  has_pass <- !is.null(passphrase)
  ext <- ifelse(has_pass, ".gpg", ".bz2")
  src <- paste0("https://archive.org/download/aquamapsdb/am.db", ext)
  temp <- file.path(dirname(tempdir()), paste0("am.db", ext))
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
    if (!has_pass) {
      R.utils::bunzip2(filename = temp, destname = tgt)
    } else {
      rcrypt::decrypt(temp, passphrase = passphrase, output = tgt)
    }
    message("done")
  } else {
    message("data appears to have been extracted already?")
  }
  return(invisible(file.exists(tgt)))
}
