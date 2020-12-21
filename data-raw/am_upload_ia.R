ia_cmd <- function(cmd)
  system(sprintf("%s %s", Sys.which("ia"), cmd), intern = TRUE)

#' Install a platform independent binary from the Internet Archive
#'
#' This tool allows for files to be archived at the Internet Archive
#'
#' @details Only tested on Linux, which install the tool into ~/bin,
#' which on for example Ubuntu means that the tool is available on
#' the default search path
#' @return invisible TRUE (if install goes well) or FALSE
#' @examples dontrun{
#' ia_install()
#' }
#' @export
ia_install <- function(install_dir) {

  is_installed <- function(x) Sys.which(x) != ""

  if (is_installed("ia")) {
    message("Done, ia already installed, version: ", ia_cmd("--version"))
    return(invisible(TRUE))
  }

  is_linux <- Sys.info()["sysname"] == "Linux"

  if (!is_linux) {
    stop("Please install manually, see: ", paste0("https://archive.org/",
      "services/docs/api/internetarchive/installation.html#binaries"))
  }

  if (missing(install_dir))
    install_dir <- "~/bin/ia"

  message("Downloading to ~/bin/ia and setting executable bit")

  download.file("https://archive.org/download/ia-pex/ia",
                destfile = install_dir)

  Sys.chmod("~/bin/ia", mode = "775")

  return(invisible(nzchar(ia_cmd("--version"))))
}

#ia_cmd("--help")
#ia_cmd("--version")
#ia_cmd("metadata aquamapsdata")

#' Upload new aquamapsdata dataset to Internet Archive
#'
#' New data is uploaded with the identifier "aquamapsdata"
#'
#' @details For an upload, the ia tool needs first to be
#' configured with a valid username and password.
#' @export
ia_upload <- function(file = paste0(am_db_sqlite(), ".gpg"))
  ia_cmd(sprintf("upload aquamapsdata %s --metadata='title:aquamapsdata'",
        file))

ia_upload_curl <- function(file = paste0(am_db_sqlite(), ".gpg")) {
  env_ia_user <- Sys.getenv("IA_ACCESS_KEY_ID")
  env_ia_pass <- Sys.getenv("IA_SECRET_ACCESS_KEY")

  if (any(nchar(c(env_ia_user, env_ia_pass)) < 1))
    stop("Please use .Renviron with IA_ACCESS_KEY_ID and IA_SECRET_ACCESS_KEY.")

  cmd <- sprintf("curl --location-trusted --header 'x-amz-auto-make-bucket:1' \
    --header 'x-archive-meta01-collection:opensource' \
    --header 'x-archive-meta-mediatype:data' \
    --header 'authorization: LOW %s:%s' \
    --upload-file /home/markus/.config/aquamaps/am.db.gpg \
    http://s3.us.archive.org/aquamapsdb/am.db.gpg",
   env_ia_user, env_ia_pass)

  system(cmd)
}

#library(rcrypt)
#encrypt(am_db_sqlite(), passphrase = "secretpassphrase")
#ia_upload_curl()
#ia_download()
#ia_cmd("list aquamaps")

ia_download_url <-
  "http://archive.org/download/aquamapsdb/am.db.gpg"
