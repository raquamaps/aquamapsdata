.onAttach <- function(lib, pkg){

  # echo "aquamapsdata" | toilet -f smblock

  welcome <-
"
                           ▌   ▐
▝▀▖▞▀▌▌ ▌▝▀▖▛▚▀▖▝▀▖▛▀▖▞▀▘▞▀▌▝▀▖▜▀ ▝▀▖
▞▀▌▚▄▌▌ ▌▞▀▌▌▐ ▌▞▀▌▙▄▘▝▀▖▌ ▌▞▀▌▐ ▖▞▀▌
▝▀▘  ▌▝▀▘▝▀▘▘▝ ▘▝▀▘▌  ▀▀ ▝▀▘▝▀▘ ▀ ▝▀▘
"
  packageStartupMessage(welcome)

    reminder <- paste0(
      "This data package requires an ",
      "Internet connection to download up-to-date data, ",
      "which will then be available locally at ", am_db_path(), "...",
      "Pls use download_db() to download the data.")

  if (!file.exists(am_db_path()))
    packageStartupMessage(reminder)

}
