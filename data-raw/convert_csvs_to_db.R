
#' Perform remote update by downloading zip with CSV and migrating files to a SQLite db stored locally
#' @param AM_SRC URL for zip of CSV files with raw data
#' @param AM_DB location of SQLite db locally
#' @param truncate boolean indicating whether to truncate CSVs
#' when converting to SQLite (used for debug and performance purposes)
#' @importFrom readxl read_excel
#' @importFrom readr read_csv write_csv write_lines read_lines cols col_character col_double
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom purrr map_lgl
#' @importFrom dplyr src_sqlite copy_to
#' @export

remote_update <- function(
  AM_SRC = "https://ftp.geomar.de/users/cgarilao/AquaMaps_ver0816.zip",
  AM_DB = paste0(system.file(package = "aquamapsdata"), "/extdata/am.db"),
  truncate = TRUE) {

  AM_ZIP <- paste0(dirname(tempdir()), "/aquamaps-dl.zip")
  #AM_ZIP <- "data-raw/aquamaps-dl.zip"
  AM_TGT <- tools::file_path_sans_ext(AM_ZIP)

  message("Remote update of aquamapsdata (",
          AM_SRC, " -> ", AM_ZIP, ")")

  # download ~400M zip (check if it doesn't already exist)
  if (!file.exists(AM_ZIP)) {
    download.file(AM_SRC, AM_ZIP)
  } else {
    message("zip appears to have been downloaded already?")
    message("proceeding to extract data ....")
  }

  # extraction takes about one minute on a MacBook Air 2012
  if (!length(dir(AM_TGT) > 0)) {
    unzip(AM_ZIP, exdir = AM_TGT)
  } else {
    message("zip appears to have been extracted already?")
  }

  # process files in .zip, start with Excel-file
  # convert it to .csv

  AM_EXCEL <- paste0(AM_TGT, "/hspen_ver0816.xlsx")
  if (file.exists(AM_EXCEL)) {
    hspen <- read_excel(AM_EXCEL,
                        col_types = c(
                          "numeric", "text", "text",
                          "text", "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "text", "date", "date", "numeric",
                          "date", "date", "numeric",
                          "numeric", "text", "numeric", "numeric",
                          "numeric", "numeric", "text", "numeric",
                          "numeric"))

    write_csv(hspen, path = paste0(AM_TGT, "/hspen.csv"))
    unlink(AM_EXCEL)
  }

  # all files from the dl are now CSV files
  csv_files <- dir(AM_TGT, pattern = "*.csv$")

  # truncation for speeding up data conversion to .sqlite
  if (truncate) {
    long <- csv_files[tools::file_ext(csv_files) == "csv"]
    short <- paste0(long, "_10k")
    cmd_truncate <- paste("head -10000", long, ">", short)
    truncate_sh <- c("#!/bin/bash", cmd_truncate)
    readr::write_lines(truncate_sh, path = paste0(AM_TGT, "/truncate.sh"))
    shell_cmd <- paste0("cd ", AM_TGT, " && chmod +x truncate.sh && ./truncate.sh")
    system(shell_cmd)
  }

  # rename files (using hardcoded filenames)

  rename_from <- paste0(AM_TGT, "/", c(
    "hcaf_species_native_ver0816.csv",
    "hcaf_v7.csv",
    "hspen.csv",
    "occurrencecells_ver0816.csv",
    "speciesoccursum_ver0816.csv"
  ))

  if (truncate)
    rename_from <- paste0(rename_from, "_10k")

  rename_to <- paste0(AM_TGT, "/", c(
    "nativemaps.csv",
    "hcaf.csv",
    "hspen.csv",
    "occ.csv",
    "taxa.csv"
  ))

  file.rename(rename_from, rename_to)

  # validate SQLite column names and migrate data to am.db

  reserved <- read_html("http://www.sqlite.org/lang_keywords.html") %>%
    html_nodes("ol li") %>% html_text

  # function to validate and fix column names in .csv files
  validate_sqlite_colnames <- function(csv_file, reserved) {
    cols <- unlist(strsplit(read_lines(csv_file, n_max = 1), ","))
    message("Cols in ", csv_file, " are ", cols)
    is_in_conflict <- any(toupper(cols) %in% reserved)
    if (is_in_conflict) {
      message("Dataset in ", csv_file, " uses reserved word")
      hit <- cols[which(toupper(cols) %in% reserved)]
      message("Changed column ", hit, " to ", paste0(hit, "_"))
      cols[which(toupper(cols) %in% reserved)] <- paste0(hit, "_")
      message("Resaving CSV ", csv_file, " with new column names")
      line_one <- paste0(collapse = ",", cols)
      all_lines <- read_lines(csv_file)
      all_lines[1] <- line_one
      write_lines(all_lines, csv_file)
    }
    return (is_in_conflict)
  }

  # process all files, replacing column names
  # that contain reserved SQLite words
  map_lgl(rename_to, function(x)
    validate_sqlite_colnames(x, rename_to))

  # parse files and migrate to SQLite db

  df_nativemaps <- read_csv(rename_to[1], col_types = "ccdll")

  df_hcaf <- read_csv(rename_to[2], col_types = cols(
    CountrySecond = col_character(),
    CountryThird = col_character()
  )
  )

  # most values in CountrySecond are numeric except "F111"?
  # df_hcaf$CountrySecond[df_hcaf$CountrySecond == "F111"] <- 111
  # df_hcaf$CountryThird[df_hcaf$CountryThird == "F111"] <- 111

  which_nonnum <- function(x) {
    badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
    which(badNum & !is.na(x))
  }

  # df_hcaf$CountryThird[which_nonnum(df_hcaf$CountryThird)]

  df_hspen <- read_csv(rename_to[3], col_types = cols(
    LandDistPrefMin = col_double(),
    LandDistPrefMax = col_double(),
    PrimProdPrefMax = col_double(),
    PrimProdPrefMin = col_double()
  )
  )

  df_occ <- read_csv(rename_to[4], col_types = NULL)
  df_taxa <- read_csv(rename_to[5], col_types = NULL)

  # migrate data into SQLite db

  if (!dir.exists(dirname(AM_DB)))
    dir.create(dirname(AM_DB))

  my_db <- src_sqlite(AM_DB, create = TRUE)

  copy_to(my_db, df_nativemaps, "nativemaps",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("SpeciesID")))

  copy_to(my_db, df_hcaf, "hcaf",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("LOICZID")))

  copy_to(my_db, df_hspen, "hspen",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("SpeciesID")))

  copy_to(my_db, df_occ, "occ",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("SpeciesID")))

  copy_to(my_db, df_taxa, "taxa",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("SPECIESID")))

  # clean up temprorary files

  unlink(AM_TGT, recursive = TRUE)

}

