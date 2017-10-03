#' Perform downloading of compressed SQLite db, storing it locally
#' @param force boolean to indicate whether to overwrite an existing db
#' @importFrom R.utils gunzip isGzipped
#' @importFrom utils download.file
#' @export
download_db <- function(force = FALSE) {

  SRC <- "http://archive.org/download/aquamapsdata/am.db.gz"
  TMP <- file.path(dirname(tempdir()), "am.db.gz")
  TGT <- file.path(system.file(package = "aquamapsdata"), "extdata", "am.db")

  # SRC <- paste0("http://archive.org/download/aquamapsdata/",
  #               "aquamapsdata_files.xml")
  # TMP <- paste0(dirname(tempdir()),
  #               "/aquamapsdata_files.xml")
  #
  # TGT <- paste0(system.file(package = "aquamapsdata"),
  #               "/extdata/aquamapsdata_files.xml")

  if (file.exists(TGT) && !force)
    stop("An existing db exists at ", TGT,
         ", to overwrite, pls rerun with force = TRUE")

  message("Download of aquamapsdata (",
            SRC, " -> ", TMP, " -> ", TGT, ")")

  if (!file.exists(TMP)) {
    download.file(SRC, TMP)
  } else {
    message("data appears to have been downloaded already?")
    message("proceeding to extract data ....")
  }

  if (isGzipped(TMP)) {
    gunzip(TMP, destname = TGT, remove = FALSE)
  } else if (!file.exists(TGT)) {
    if (!dir.exists(dirname(TGT)))
      dir.create(dirname(TGT))
    res <- file.copy(TMP, TGT)
  } else {
    message("data appears to have been extracted already?")
  }
  message("done updating.")
}

