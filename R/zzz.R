.onAttach <- function(lib, pkg) {

  # echo "aquamapsdata" | toilet -f smblock

  welcome <-
"
  _.  _.      _. ._ _   _. ._   _  _|  _. _|_  _.
 (_| (_| |_| (_| | | | (_| |_) _> (_| (_|  |_ (_|
       |                   |
"

# "
#
#                                   ▐      ▗
#  ▄▖  ▄▄ ▗ ▗  ▄▖ ▗▄▄  ▄▖ ▗▄▖  ▄▖  ▄▟  ▄▖ ▗▟▄  ▄▖
# ▝ ▐ ▐▘▜ ▐ ▐ ▝ ▐ ▐▐▐ ▝ ▐ ▐▘▜ ▐ ▝ ▐▘▜ ▝ ▐  ▐  ▝ ▐
# ▗▀▜ ▐ ▐ ▐ ▐ ▗▀▜ ▐▐▐ ▗▀▜ ▐ ▐  ▀▚ ▐ ▐ ▗▀▜  ▐  ▗▀▜
# ▝▄▜ ▝▙█ ▝▄▜ ▝▄▜ ▐▐▐ ▝▄▜ ▐▙▛ ▝▄▞ ▝▙█ ▝▄▜  ▝▄ ▝▄▜
#       ▐                 ▐
#       ▝                 ▝
#
# "
  packageStartupMessage(welcome)

    reminder <- paste0(
      "This data package requires an ",
      "Internet connection to download up-to-date data, ",
      "which will then be available locally at ", am_db_sqlite(), "...",
      "Pls use download_db() to download the data.")

    if (!file.exists(aquamapsdata::am_db_sqlite())) {
      mini_db <- file.path(system.file(package = "aquamapsdata"), "extdata", "am.db")
      packageStartupMessage("Temporarily using bundled minified db at ", mini_db)
      if (!file.exists(mini_db)) {
        mini_db <- file.path(system.file(package = "aquamapsdata"), "inst", "extdata", "am.db")
        packageStartupMessage("Couldn't find mini db, attempting with ", mini_db)
      }
      if (!file.exists(mini_db)) {
        mini_db <- file.path(system.file(package = "aquamapsdata"), "am.db")
        packageStartupMessage("Couldn't find mini db, attempting with ", mini_db)
      }
      if (!dir.exists(basename(aquamapsdata::am_db_sqlite())))
        dir.create(basename(aquamapsdata::am_db_sqlite()), recursive = TRUE, showWarnings = TRUE)
      file.copy(mini_db, aquamapsdata::am_db_sqlite())
      packageStartupMessage("Pls remember to use download_db() to use real data...")
    }

}
