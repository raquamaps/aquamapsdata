.onAttach <- function(lib, pkg){

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

  if (!file.exists(am_db_sqlite())) {
    packageStartupMessage("Temporarily using bundled minified db...")
    mini_db <- file.path(system.file(package = "aquamapsdata"), "extdata", "am.db")
    file.copy(mini_db, am_db_sqlite())
    packageStartupMessage(reminder)
  }

}

