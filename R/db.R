#' Connection to aquamapsdata
#'
#' This function returns a db connection to a pre-configured
#' data source containing aquamaps data
#'
#' @param source_type one of "sqlite", "duckdb", "mysql" or "extdata",
#'  with "sqlite" being default
#' @return database connection
#' @examples \dontrun{
#' library(DBI)
#' con <- con_am("sqlite")
#' dbDisconnect(con)
#' }
#' @export
#' @family admin
con_am <- function(source_type = c("sqlite", "duckdb", "mysql", "extdata")) {
  type <- match.arg(source_type)
  switch(type,
     sqlite = con_am_sqlite(),
     duckdb = con_am_duckdb(),
     mysql = con_am_mysql(),
     extdata = con_am_extdata()
  )
}

db_env <- function() {

  envvars <- c("AM_DBHOST", "AM_DBNAME", "AM_DBUSER", "AM_DBPASS")

  if (any(Sys.getenv(envvars) == "")) {

    message("Do you have an .Renviron file? If not use \nfile.edit('",
      normalizePath("~/.Renviron"), "')\nreadRenviron('~/.Renviron')")

    stop("Please use an .Renviron with these envvars set: ",
         paste(envvars, collapse = ", "))
  }
  as.list(Sys.getenv(envvars))
}

#' Connection to AquaMaps data source using RMySQL
#'
#' This function relies on an .Renviron file with environment variables for
#' a connection to the MySQL data source. Make sure one exists and
#' that variables are set for: AM_DBHOST, AM_DBNAME, AM_DBUSER, AM_DBPASS
#'
#' @importFrom DBI dbConnect
#' @importFrom RMySQL dbConnect MySQL
#' @noRd
#' @family admin
con_am_mysql <- function() {

  cs <- db_env()

  RMySQL::dbConnect(RMySQL::MySQL(fetch.default.rec = 1e4),
   host = cs$AM_DBHOST, dbname = cs$AM_DBNAME,
   user = cs$AM_DBUSER, password = cs$AM_DBPASS
  )
}

#' Location of sqlite3 db file
#' @return character string representing on disk location for db file
#' @importFrom rappdirs app_dir
#' @export
#' @family admin
am_db_sqlite <- function() {
  file.path(rappdirs::app_dir("aquamaps")$config(), "am.db")
}

#' Location of duck db file
#' @return character string representing on disk location for db file
#' @importFrom rappdirs app_dir
#' @export
#' @family admin
am_db_duckdb <- function() {
  file.path(rappdirs::app_dir("aquamaps")$config(), "am.duck")
}

#' Connection to AquaMaps data source using SQLite3 db
#'
#' This function relies on a "am.db" file being present in the relevant
#' application directory for a connection to the SQLite3 data source.
#' @importFrom RSQLite SQLITE_RWC SQLITE_RW SQLite
#' @importFrom DBI dbConnect
#' @importFrom rappdirs app_dir
#' @importFrom readr write_file
#' @noRd
#' @family admin
con_am_sqlite <- function(create = FALSE, overwrite = FALSE, copy_from_raw) {

  db_path <- am_db_sqlite()

  if (!file.exists(db_path) & !create)
    stop("No sqlite3 db available at ", db_path)

  if (file.exists(db_path) & create & !overwrite)
    stop("A file exists at ", db_path,
      ", use `overwrite` = TRUE to overwrite it.")

  if (file.exists(db_path) & create & overwrite) {
    message("Deleting database at ", db_path,
      ", creating new empty database there.")
    unlink(db_path)
  }

  if (!file.exists(dirname(db_path)) & create) {
    message("Creating local dir for sqlite3 db at ", dirname(db_path))
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
    if (!missing(copy_from_raw)) {
      message("Creating db based on raw copy... ")
      # this may not run during "staged installation"
      # where no files can be written anywhere in the fs
      readr::write_file(copy_from_raw, db_path)
    }
  }

  sqliteflag <- if (create) RSQLite::SQLITE_RWC else RSQLite::SQLITE_RW

  DBI::dbConnect(RSQLite::SQLite(), synchronous = "normal",
    dbname = db_path, flags = sqliteflag)

}

#' Connection to AquaMaps data source using SQLite3 db
#'
#' This function relies on a bundled minified "am.db" file being bundled in the
#' R package (inst/extdata directory) for a connection to the
#' SQLite3 data source.
#'
#' @importFrom RSQLite SQLITE_RO SQLite
#' @importFrom DBI dbConnect
#' @noRd
#' @family meta
con_am_extdata <- function() {
  extdata <-
    system.file("extdata", "am.db", package = "aquamapsdata", mustWork = TRUE)
  DBI::dbConnect(RSQLite::SQLite(), extdata, flags = RSQLite::SQLITE_RO)
}

#' Connection to AquaMaps data source using duck db
#'
#' This function relies on a "am.duck" file being present in the
#' relevant application directory for a connection to the duck db data source.
#'
#' @importFrom duckdb dbConnect duckdb
#' @noRd
#' @family meta
con_am_duckdb <- function(create = FALSE, overwrite = FALSE) {

  db_path <- am_db_duckdb()

  if (!file.exists(db_path) & !create)
    stop("No duck db available at ", db_path)

  if (file.exists(db_path) & create & !overwrite)
    stop("A file exists at ", db_path,
      ", use `overwrite` = TRUE to overwrite it.")

  if (file.exists(db_path) & create & overwrite) {
    message("Deleting database at ", db_path,
      ", creating new empty database there.")
    unlink(db_path)
  }

  if (!file.exists(dirname(db_path)) & create) {
    message("Creating local dir for sqlite3 db at ", dirname(db_path))
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  }

  duckdb::dbConnect(duckdb::duckdb(), db_path)
}
#' Summary with total row counts for a db connection and a set of tables
#' @importFrom DBI dbListTables
#' @importFrom purrr map_df
#' @noRd
#' @family admin
db_counts <- function(con, tables) {

  if (missing(tables))
    tables <- DBI::dbListTables(con)

  # fcn to count nr of rows in a db table
  df_rowcount <- function(x)
    tbl(con, x) %>%
    summarize(count = n()) %>%
    collect() %>%
    mutate(table = x)

  # fcn to count nr of cols in a db table
  df_colcount <- function(x) tibble(
    n_cols = tbl(con, x) %>% ncol(),
    table = x
  )

  # for all enumerated tables, count rows and cols
  n_rows <- purrr::map_df(tables, df_rowcount)
  n_cols <- purrr::map_df(tables, df_colcount)

  # compile summary results
  n_rows %>%
    left_join(n_cols, by = "table") %>%
    select(table, everything()) %>%
    arrange(desc(n_rows))

}

#' @family admin
db_tables <- function(con) {

  if (missing(con)) {
    con <- con_am("sqlite")
    on.exit(dbDisconnect(con))
  }

  type <- class(con)[1]

  source_type <- switch(type,
    "MySQLConnection" = "mysql",
    "SQLiteConnection" = "sqlite",
    "duckdb_connection" = "duckdb")

  if (!source_type %in% c("mysql", "sqlite", "duckdb"))
    stop("Only mysql, sqlite3 and duckdb connections are supported.")

  enum_tables_mysql <- function() {
    tables <- dbListTables(con)
    if (!length(tables)) return(NULL)
    res <- db_counts(con, tables)
    return(res)
  }

  enum_tables_sqlite <- function() {
    mygrep <- function(x, pattern = "^sqlite_")
      grep(x = x, pattern = pattern, invert = TRUE, value = TRUE)
    tables <- RSQLite::dbListTables(con) %>% mygrep()
    if (length(tables) > 0)
      res <- db_counts(con, tables) else res <- NULL
    return(res)
  }

  enum_tables_duckdb <- function() {
    tables <- duckdb::dbListTables(con)
    if (length(tables) > 0)
      res <- db_counts(con, tables) else res <- NULL
    return(res)
  }

  switch(source_type,
    mysql = enum_tables_mysql(),
    sqlite = enum_tables_sqlite(),
    duckdb = enum_tables_duckdb()
  )

}

#' @importFrom DBI dbIsValid dbDisconnect
#' @noRd
#' @family admin
db_reconnect <- function(con) {

  if (DBI::dbIsValid(con)) return(con)

  source_type <- switch(class(con),
    "MySQLConnection" = "mysql",
    "SQLiteConnection" = "sqlite",
    "duckdb_connection" = "duckdb")

  if (!source_type %in% c("mysql", "sqlite", "duckdb"))
    stop("Only mysql, sqlite3 and duckdb connections are supported.")

  if (!DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
    con <- con_am(source_type)
  }

  message("reconnected to ", source_type)

  con
}

#' @importFrom purrr as_vector
#' @importFrom DBI dbRemoveTable dbFetch dbIsValid dbWriteTable dbClearResult
#' @noRd
#' @family admin
db_sync_table <- function(
  table, chunk_size = 1e4,
  con_src, con_dest, overwrite = FALSE) {

  if (missing(con_src)) {
    con_src <- con_am("mysql")
    on.exit(dbDisconnect(con_src))
  }

  if (missing(con_dest)) {
    con_dest <- con_am("sqlite")
    on.exit(dbDisconnect(con_dest))
  }

  tables_src <- db_tables(con_src)$table
  tables_dest <- db_tables(con_dest)$table

  if (!table %in% tables_src)
    stop("Table ", table, " is not available in the source connection.")

  if (table %in% tables_dest & !overwrite)
    stop("Table ", table, " is in the destination connection, ",
      "use `overwrite = TRUE`")

  if (table %in% tables_dest & overwrite)
    message("\nTable ", table, " will be overwritten at the ",
      "destination connection")

  rc_sql <- sprintf("SELECT COUNT(*) as n FROM %s;", table)
  rc <- dbGetQuery(con_src, rc_sql) %>% purrr::as_vector()
  p <- progress_estimated(n = ceiling(rc / chunk_size))

  rs_sql <- sprintf("SELECT * FROM %s;", table)
  rs <- dbSendQuery(con_src, rs_sql)

  iter <- 0
  if (overwrite) DBI::dbRemoveTable(con_dest, table)
  while (!dbHasCompleted(rs)) {
    iter <- iter + 1
    chunk <- DBI::dbFetch(rs, chunk_size) %>% as_tibble()
    # HACK it seems the connection can auto-disconnect
    if (!DBI::dbIsValid(con_src)) con_src <- db_reconnect(con_src)
    if (!DBI::dbIsValid(con_dest)) con_dest <- db_reconnect(con_dest)
    DBI::dbWriteTable(con_dest, table, chunk, append = TRUE)
    p$pause(0.1)$tick()$print()
    if (iter %% 1e2 == 0) message("Rows fetched: ", iter * chunk_size)
  }
  DBI::dbClearResult(rs)

}

#' Sync a MariaDB/MySQL database to a local SQLite3 db
#'
#' This function syncs db tables from a mysql source db and
#' writes the data into a local SQLite3 db using buffering, with
#' chunk size set to 1e4 items per chunk, in order to avoid out of
#' memory exceptions when moving large tables.
#'
#' @param tables_included a vector of table names in the source db to be
#'   included, by default all tables are included except those excluded
#' @param tables_excluded a vector of table names in the source db to be
#'   excluded, by default a number of tables are excluded, specify NULL
#'   to not explicitly exclude any tables
#' @param overwrite_existing a logical to indicate whether destination tables
#'   should be overwritten if they already exist
#' @param con_src db connection to source db
#' @param con_dest db connection to destination db
#' @param overwrite_existing logical to indicate if existing tables at
#' destination db should be overwritten, Default: FALSE
#' @return invisible result with vector of boolean status flags for
#' synced tables
#' @importFrom purrr map set_names
#' @importFrom DBI dbDisconnect
#' @importFrom stringr str_starts
#' @importFrom dplyr pull
#' @export
#' @family admin
db_sync <- function(tables_included,
  tables_excluded = c("hcaf_species_native", "occurrencecells_r"),
  con_src, con_dest, overwrite_existing = FALSE) {

  if (missing(con_src)) {
    con_src <- con_am("mysql")
    on.exit(dbDisconnect(con_src))
  }

  if (missing(con_dest)) {
    con_dest <- con_am("sqlite")
    on.exit(dbDisconnect(con_dest))
  }

  c1 <- con_src

  if (missing(tables_included)) {
    t1 <- c1 %>% db_tables() %>% pull(table)
  } else {
    t1 <- tables_included
  }

  tryCatch(
    c2 <- con_dest,
    error = function(e) {
      if (str_starts(e$message, "No sqlite3 db")) {
        message("No sqlite3 db exists, probably first run, so creating one.")
        c2 <- con_am_sqlite(create = TRUE)
      }
    }
  )

  t2_df <- c2 %>% db_tables()
  t2 <- if (is.null(t2_df)) NULL else t2_df %>% pull(table)

  # inclusions
  if (overwrite_existing)
    # which tables exist in both src and dest dbs?
    delta <- intersect(t1, t2)
  else
    # which tables are new, ie only exist in src db?
    delta <- setdiff(t1, t2)

  # exclusions
  tables <- setdiff(delta, tables_excluded)

  # safe function for syncing tables
  sync_possibly <-  purrr::possibly(
    .f = function(x) db_sync_table(x, overwrite = overwrite_existing,
      con_src = con_src, con_dest = con_dest),
    otherwise = FALSE)

  # iterate over all tables for side-effects of synching
  message("excluded tables: ", paste(tables_excluded, collapse = ", "))
  message("syncing these tables from source db:\n",
    if (length(tables) > 0) paste(collapse = ", ", tables) else "no sync need")

  res <- purrr::map_lgl(tables, sync_possibly)
  names(res) <- as.character(tables)
  message("done")
  invisible(res)
}

#' Location of minified bundled sqlite db
#'
#' A minified sqlite db is bundled in the package for technical reasons.
#' @export
#' @family admin
db_minify_path <- function()
  system.file("extdata", "am.db", package = "aquamapsdata", mustWork = TRUE)

#' Minify a larger aquamaps dataset by slicing
#'
#' Creates a minimized version of a larger AquaMaps database,
#' by filtering all tables to contain a single identifier.
#' @param key the species identifier to keep
#' @param slice_file the file path for storing the resulting SQLite3 db
#' @param chunk_size for chunked data transfer, default: 1e4
#' @export
#' @importFrom DBI dbDisconnect
#' @importFrom RSQLite dbConnect SQLite dbDisconnect dbWriteTable
#' @importFrom purrr iwalk
#' @importFrom dplyr tbl filter distinct pull
#' @family admin
db_minify <- function(key, slice_file, chunk_size = 1e4) {

  con <- con_am("sqlite")
  on.exit(DBI::dbDisconnect(con))

  csc <-
    con %>%
    tbl("hcaf_species_native") %>%
    filter(.data$SpeciesID %in% key) %>%
    distinct(.data$CsquareCode) %>%
    pull(.data$CsquareCode)

  hcaf_r <-
    con %>%
    tbl("hcaf_r") %>%
    filter(.data$CsquareCode %in% csc)

  tbls <-
    aquamapsdata::am_meta %>%
    select(.data$table) %>%
    pull(.data$table) %>%
    unique()

  ft <- function(x)
    con %>%
    tbl(x) %>%
    filter(.data$SpeciesID %in% key) #%>%
  #    collect()

  other <- tbls[2:5] %>% map(ft)
  names(other) <- tbls[2:5]

  am_slice <- c(hcaf_r = list(hcaf_r), other)

  con_dest <- RSQLite::dbConnect(RSQLite::SQLite(), slice_file)
  on.exit(RSQLite::dbDisconnect(con_dest))

  wt <- function(x, y)
    con_dest %>% RSQLite::dbWriteTable(name = y, value = x)

  capture_query <- function(df) {
    out <- capture.output(df %>% dplyr::show_query())
    paste(tail(out, -1), collapse = "")
  }

  wt2 <- function(x, y) {
    table <- y
    rc <- x %>% summarize(count = n()) %>%
      collect() %>% pull(count) #nrow() %>% collect() #purrr::as_vector()
    ticks <- ceiling(rc / chunk_size)
    p <- dplyr::progress_estimated(n = ticks, min_time = 1)
    rs <- DBI::dbSendQuery(x$src$con, capture_query(x))
    iter <- 0
    is_done <- FALSE
    while (!is_done) {
      iter <- iter + 1
      chunk <- DBI::dbFetch(rs, chunk_size) %>% as_tibble()
      DBI::dbWriteTable(con_dest, table, chunk, append = TRUE)
      if (iter %% 1e2 == 0)
        message("Fetched from ", table, ": ", iter * chunk_size)
      is_done <- (iter * chunk_size) >= rc
      p$pause(0.1)$tick()$print()
    }
    DBI::dbClearResult(rs)
  }

  am_slice %>% purrr::iwalk(wt2)

}

#' Staged install workaround to install temporary extdata
#'
#' This function is used to support CI with staged installation and
#' building of vignettes before package gets finally installed; it uses
#' data from a minified aquamapsdb bundled in the package at "inst/extdata".
#'
#' @export
#' @importFrom readr read_file_raw write_file
#' @family admin
am_use_offline_db <- function() {

  if (file.exists(am_db_sqlite())) {
    message("Skipping, db already found at ", am_db_sqlite())
    return(invisible(FALSE))
  }

  offline_db <-
    system.file("extdata", "am.db",
                package = "aquamapsdata", mustWork = TRUE)

  if (!dir.exists(basename(am_db_sqlite()))) {
    message("Creating local dir for sqlite3 db at ", dirname(offline_db))
    dir.create(basename(am_db_sqlite()), recursive = TRUE, showWarnings = TRUE)
  }

  readr::write_file(readr::read_file_raw(offline_db), am_db_sqlite())
  con <- con_am()
  is_valid <- RSQLite::dbIsValid(con)
  on.exit(RSQLite::dbDisconnect(con))
  return(invisible(is_valid))
}

#' Set or switch the default database used
#'
#' The database connection used when a specific connection is not provided
#' can be set with this function. It can also be switched.
#' @details the "extdata" source refers to a minified < 1MB sqlite3 db
#' with a very small subset of the full data, which is bundled into the
#' package and which allows tests and vignettes to run in the package without
#' having the full dataset installed locally (during for staged installation).
#' @param source string, one of "sqlite", "duckdb", "mysql" or "extdata"
#' @export
#' @family admin
default_db <- function(source = Sys.getenv("AM_DB_SOURCE")) {

  db <- mget("local_db", envir = db_cache, ifnotfound = NA)[[1]]
  src <- mget("source", envir = db_cache, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    if (DBI::dbIsValid(db) & source == src) {
      return(db)
    }
  }
  db <- con_am(source_type = source)
  assign("local_db", db, envir = db_cache)
  assign("source", source, envir = db_cache)
  db
}

#' Disconnect the default database connection
#' @param env the environment holding the connection, by default db_cache
#' @export
#' @family admin
db_disco <- function(env = db_cache) {
  db <- mget("local_db", envir = env, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    suppressWarnings(
      DBI::dbDisconnect(db)
    )
  }
}

# Environment to store the cached copy of the connection
# and a finalizer to close the connection on exit.
db_cache <- new.env()
reg.finalizer(db_cache, db_disco, onexit = TRUE)
