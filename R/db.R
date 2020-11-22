#' Connection to aquamapsdata
#'
#' This function returns a db connection to one of two possible pre-configured
#' data sources containing aquamaps data
#'
#' @param source_type one of "duckdb", "sqlite" or "mysql" with "sqlite" being default
#' @return database connection
#' @export
con_am <- function(source_type = c("sqlite", "mysql", "duckdb"))
{
  type <- match.arg(source_type)
  switch(type,
     mysql = con_am_mysql(),
     sqlite = con_am_sqlite(),
     duckdb = con_am_duckdb()
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
con_am_mysql <- function() {

  cs <- db_env()

  RMySQL::dbConnect(RMySQL::MySQL(fetch.default.rec = 1e4),
   host = cs$AM_DBHOST, dbname = cs$AM_DBNAME,
   user = cs$AM_DBUSER, password = cs$AM_DBPASS
  )
}

#' Location of sqlite3 db file
#'
#' @export
#' @return character string representing on disk location for db file
#' @importFrom rappdirs app_dir
#'
am_db_sqlite <- function() {
  file.path(rappdirs::app_dir("aquamaps")$config(), "am.db")
}

#' Location of duck db file
#'
#' @export
#' @return character string representing on disk location for db file
#' @importFrom rappdirs app_dir
#'
am_db_duckdb <- function() {
  file.path(rappdirs::app_dir("aquamaps")$config(), "am.duck")
}

#' Connection to AquaMaps data source using SQLite3 db
#'
#' This function relies on a "am.db" file being present in the relevant application
#' directory for a connection to the SQLite3 data source.
#'
#' @importFrom RSQLite SQLITE_RWC SQLITE_RW SQLite
#' @importFrom DBI dbConnect
#' @importFrom rappdirs app_dir
#' @noRd
con_am_sqlite <- function(create = FALSE, overwrite = FALSE)
{
  db_path <- am_db_sqlite()

  if (!file.exists(db_path) & !create)
    stop("No sqlite3 db available at ", db_path)

  if (file.exists(db_path) & create & !overwrite)
    stop("A file exists at ", db_path, ", use `overwrite` = TRUE to overwrite it.")

  if (file.exists(db_path) & create & overwrite) {
    message("Deleting database at ", db_path, ", creating new empty database there.")
    unlink(db_path)
  }

  if (!file.exists(dirname(db_path)) & create) {
    message("Creating local dir for sqlite3 db at ", dirname(db_path))
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  }

  sqliteflag <- if (create) RSQLite::SQLITE_RWC else RSQLite::SQLITE_RW

  DBI::dbConnect(RSQLite::SQLite(), synchronous = "normal",
    dbname = db_path, flags = sqliteflag)

}

#' Connection to AquaMaps data source using duck db
#'
#' This function relies on a "am.duck" file being present in the relevant application
#' directory for a connection to the duck db data source.
#'
#' @importFrom duckdb dbConnect duckdb
#' @noRd
con_am_duckdb <- function(create = FALSE, overwrite = FALSE) {

  db_path <- am_db_duckdb()

  if (!file.exists(db_path) & !create)
    stop("No duck db available at ", db_path)

  if (file.exists(db_path) & create & !overwrite)
    stop("A file exists at ", db_path, ", use `overwrite` = TRUE to overwrite it.")

  if (file.exists(db_path) & create & overwrite) {
    message("Deleting database at ", db_path, ", creating new empty database there.")
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
db_counts <- function(con, tables) {

  if (missing(tables))
    tables <- DBI::dbListTables(con)

  # fcn to count nr of rows in a db table
  df_rowcount <- function(x)
    tbl(con, x) %>%
    #count() %>% collect() %>%
    #rename(n_rows = n) %>%
    summarize(count = n()) %>% collect() %>%
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
    return (res)
  }

  enum_tables_duckdb <- function() {
    tables <- duckdb::dbListTables(con)
    if (length(tables) > 0)
      res <- db_counts(con, tables) else res <- NULL
    return (res)
  }

  switch(source_type,
    mysql = enum_tables_mysql(),
    sqlite = enum_tables_sqlite(),
    duckdb = enum_tables_duckdb()
  )

}

#' @importFrom DBI dbIsValid dbDisconnect
#' @noRd
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
db_sync_table <- function(
  table, chunk_size = 1e4,
  con_src, con_dest, overwrite = FALSE)
{
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
    stop("Table ", table, " is in the destination connection, use `overwrite = TRUE`")

  if (table %in% tables_dest & overwrite)
    message("\nTable ", table, " will be overwritten at the destination connection")

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

#' Sync the MySQL database to a local SQLite3 db
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
#' @return invisible result with vector of boolean status flags for synced tables
#' @importFrom purrr map set_names
#' @importFrom DBI dbDisconnect
#' @importFrom stringr str_starts
#' @importFrom dplyr pull
#' @export
db_sync <- function(tables_included,
  tables_excluded = c("hcaf_species_native", "occurrencecells_r"),
  con_src, con_dest, overwrite_existing = FALSE)
{

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
    if (length(tables) > 0) paste(collapse = ", ", tables) else "nothing to sync")

  res <- purrr::map_lgl(tables, sync_possibly)
  names(res) <- as.character(tables)
  message("done")
  invisible(res)
}

#' @importFrom DBI dbDisconnect dbExistsTable dbExecute dbGetQuery
#' @noRd
am_create_fts <- function(con, overwrite = TRUE) {

  if (missing(con)) {
    con <- con_am("sqlite")
    on.exit(DBI::dbDisconnect(con))
  }

  if (overwrite)
    if (DBI::dbExistsTable(con, "fts"))
      am_drop_fts()

  if (!"fts" %in% dbListTables(con)) {

    DBI::dbExecute(con, statement =
      "create virtual table fts using fts5(
        key, terms
      );")

    DBI::dbExecute(con, statement = paste0(
      "insert into fts select SpeciesID as key, printf('",
        paste(collapse = " ", rep("%s", 8)), "',
          `Genus`, `Species`, `FBname`,
          `Kingdom`, `Phylum`, `Class`,
          `Order`, `Family`
          ) as terms FROM speciesoccursum_r;"))
    }
    n_keys <- DBI::dbGetQuery(con, "select count(*) from fts;")
    message("Added FTS index for ", n_keys, " keys.")
    # res <- am_name_search() %>% collect %>%
    #   tidyr::unite(terms, -key, sep = " ") %>%
    #   select(key, terms)
    #
    # RSQLite::dbWriteTable(src_sqlite_aquamapsdata(),
    #  "fts", res, overwrite = FALSE, row.names = FALSE)
}

#' Fuzzy search for terms related to taxonomic names
#' @param search_term token query, phrase query or NEAR query
#' (see http://www.sqlite.org/fts5.html)
#' @return tibble with matching keys (database identifiers)
#' @examples
#'  am_search_fuzzy("cod")
#'  am_search_fuzzy("cod OR shark")
#' @export
am_search_fuzzy <- function(search_term) {

  query <- paste("select key, terms from fts",
   sprintf("where terms match '%s'", search_term))
  res <- am_custom_query(query)
  #return(as_tibble(res))
  return (res)
}

#' Run a custom SQL query
#' @param sql_query the query
#' @param con the connection to use, if missing an sqlite con is used
#' @param ... other arguments to be passed to the sql() fcn
#' @importFrom dplyr sql tbl collect
#' @importFrom DBI dbDisconnect
#' @export
am_custom_query <- function(sql_query, con, ...){

  if (missing(con)) {
    con <- con_am("sqlite")
    on.exit(DBI::dbDisconnect(con))
  }

  con %>%
    tbl(sql(sql_query, ...)) %>%
    collect()

}

#' @importFrom DBI dbRemoveTable
#' @importFrom purrr map
#' @noRd
am_drop_fts <- function(con) {

  if (missing(con)) {
    con <- con_am("sqlite")
    on.exit(DBI::dbDisconnect(con))
  }

  fts_shadow_tables <- function(tablename)
    sprintf("%s_%s", tablename, c("data", "idx", "config", "content", "docsize"))

  drop_table <- function(x)
    DBI::dbRemoveTable(con, x, fail_if_missing = FALSE)

  fts_tabs <- c("fts", fts_shadow_tables("fts"))

  fts_tabs %>% purrr::map(drop_table)

}

#' Exact search for taxonomic names
#' @param SpeciesID AquaMaps unique identifier for a valid species used by the Catalogue of Life Annual Checklist (www.catalogueoflife.org). Example for the whale shark: Fis-30583
#' @param SpecCode Species identifier used in FishBase or SeaLifeBase
#' @param Genus Genus name of the species
#' @param Species Specific epithet of the species
#' @param FBname Common name suggested by FishBase or SeaLifeBase
#' @param OccurRecs Number of point records used to generate good cells
#' @param OccurCells Number of good cells used to generate species envelope
#' @param StockDefs Distribution of the species as recorded in FishBase or SeaLifeBase
#' @param Kingdom Kingdom to which the species belongs
#' @param Phylum Phylum to which the species belongs
#' @param Class Class to which the species belongs
#' @param Order Order to which the species belongs
#' @param Family Family to which the species belongs
#' @param deepwater Does the species occur in the deep-sea (i.e. tagged  bathypelagic or bathydemersal in FishBase or SeaLifeBase)? 0=No, 1=Yes
#' @param angling Is the species a sport fish (i.e. tagged as a GameFish in FishBase)? 0=No, 1=Yes
#' @param diving Is the species found on a dive (i.e. where DepthPrefMin in HSPEN < 20 meters)? 0=No, 1=Yes
#' @param dangerous Is the species dangerous (i.e. tagged as traumatogenic or venonous in FishBase or SeaLifeBase)? 0=No, 1=Yes
#' @param m_invertebrates Is the species a marine invertebrate? 0=No, 1=Yes
#' @param highseas Is the species an open ocean fish species (i.e. tagged as pelagic-oceanic in FishBase)? 0=No, 1=Yes
#' @param invasive Is the species recorded to be invasive (i.e. in FishBase or SeaLifeBase)? 0=No, 1=Yes
#' @param resilience Resilience of the species (i.e. as recorded in FishBase/SeaLifeBase)
#' @param iucn_id IUCN species identifier
#' @param iucn_code IUCN Red list classification assigned to the species
#' @param iucn_version IUCN version
#' @param provider FishBase (FB) or SeaLifeBase (SLB)?
#' @return tibble with matching identifiers...
#' @export
am_search_exact <- function(
  SpeciesID=NULL, SpecCode=NULL, Genus=NULL, Species=NULL, FBname=NULL, OccurRecs=NULL, OccurCells=NULL, StockDefs=NULL, Kingdom=NULL, Phylum=NULL, Class=NULL, Order=NULL, Family=NULL, deepwater=NULL, angling=NULL, diving=NULL, dangerous=NULL, m_invertebrates=NULL, highseas=NULL, invasive=NULL, resilience=NULL, iucn_id=NULL, iucn_code=NULL, iucn_version=NULL, provider=NULL){

  args <-  as.list(match.call(expand.dots = TRUE)[-1])

  tc <- function(l) Filter(Negate(is.null), l)
  tcn <- function(l) Filter(is.null, l)

  if (length(tcn(args)) > 0) {
    nullfields <- paste0(collapse = ", ", names(tcn(args)))
    message("Got NULL params for ", nullfields)
    message("Showing some example combinations")
    sql <- sprintf("select distinct * from (select %s from speciesoccursum_r limit 100) limit 10", nullfields)
    return(am_custom_query(sql))
  }

  if (!(length(tc(args)) > 0))
    return(am_custom_query("select * from speciesoccursum_r"))

  dbq <- function(x)
    DBI::dbQuoteString(DBI::ANSI(), DBI::dbQuoteString(DBI::ANSI(), DBI::SQL(x)))

  sql_where <- function(x)
    sprintf('%s = "%s"', names(x), dbq(x))

  sql_where <- paste(collapse = " and ", sql_where(tc(args)))
  sql <- sprintf("select * from speciesoccursum_r where %s", sql_where)
  am_custom_query(sql)
}

#' @importFrom DBI dbDisconnect dbExecute
#' @noRd
am_create_indexes <- function(con) {

  if (missing(con)) {
    con <- con_am("sqlite")
    on.exit(DBI::dbDisconnect(con))
  }

  i <-
    aquamapsdata::am_meta %>%
    filter(.data$field %in% c("SpeciesID", "LOICZID")) %>%
    select(vars("table", "field"))

  idx <- paste0(i$field, 1:length(i$field))
  sql <- sprintf("create index if not exists [%s] on %s([%s])", idx, i$table, i$field)
  execute <- function(x) {
    res <- DBI::dbExecute(con, x)
    message("Index created: ", res, ", sql: ", x)
    res
  }
  res <- sql %>% map(execute)

  if (!all(res == 0)) message("Done")
}

am_keys <- function() {
  con <- con_am("sqlite")
  on.exit(DBI::dbDisconnect(con))
  con %>% dplyr::tbl("speciesoccursum_r") %>%
    dplyr::distinct(.data$SpeciesID) %>%
    dplyr::collect() %>%
    dplyr::pull(.data$SpeciesID) #%>% dplyr::collect() %>%
#    dplyr::filter(grepl("Fis", dplyr::vars("SpeciesID"))) %>%
#    .$SpeciesID
}

#' @importFrom rlang .data
#' @noRd
am_nativemaps <- function(key) {

  if (!all(key %in% am_keys()))
    stop("Please specify valid key(s)")

  con <- con_am("sqlite")
  on.exit(dbDisconnect(con))

  vals <-
    con %>% dplyr::tbl("hcaf_species_native") %>%
    dplyr::filter(.data$SpeciesID %in% key) %>%
    dplyr::collect() %>%
    dplyr::select(.data$SpeciesID, .data$CenterLat, .data$CenterLong, .data$Probability) %>%
    dplyr::rename(lat = "CenterLat", lon = "CenterLong")

  #  coords <-
  #    vals %>%
  #    pull(CsquareCode) %>%
  #    map_df(csquare_to_dd) %>% collect()

  #  bind_cols(vals, coords)
  vals
}

#' Raster with native habitat map data for one or several species
#' @details if the fun parameter is omitted, the probability field will be
#' used
#' @param key a string identifier for the species
#' @param resolution the grid resolution in degress, by default 0.5
#' @param fun the aggregation fun to use when grid cells are
#' covered by multiple spatial features, by default their count
#' @importFrom raster raster rasterize extent
#' @export
am_raster <- function(key, resolution = 0.5, fun = c("count", "last", "first")) {

  nm <- am_nativemaps(key)
  nm$resolution <- resolution
  nr <- diff(range(nm$lat)) / unique(nm$resolution)
  nc <- diff(range(nm$lon)) / unique(nm$resolution)
  ext <- raster::extent(min(nm$lon), max(nm$lon), min(nm$lat), max(nm$lat))
  #  ext <- raster::extent(min(nm$lat), max(nm$lat), min(nm$lon), max(nm$lon))
  #  r1 <- raster::raster(nrows = nc, ncols = nr, ext = ext, resolution = nm$resolution)
  r1 <- raster::raster(nrows = nr, ncols = nc, ext = ext, resolution = nm$resolution)

  if (missing(fun))
    return(raster::rasterize(tibble(nm$lon, nm$lat), r1, field = nm$Probability))

  raster::rasterize(tibble(nm$lon, nm$lat), r1, fun = fun)
}

#' Map for native habitat
#'
#' Shows a leaflet map for the native habitat given a species identifier
#' @param ras a raster object, for example from the am_raster function
#' @param title a string to use as legend title, by default blank
#' @param cols a vector with three hex colors to use for a numeric color
#' legend, default:
#' @export
#' @importFrom leaflet leaflet addTiles addRasterImage addLegend colorNumeric projectRasterForLeaflet
#' @importFrom raster values
am_map_leaflet <- function(ras, title = "",
  cols = c("#FFFFCC", "#0C2C84", "#41B6C4")) {

  ras <- leaflet::projectRasterForLeaflet(ras, method = "bilinear")

  pal <- leaflet::colorNumeric(cols,
    raster::values(ras),
    na.color = "transparent")

  leaflet() %>% addTiles() %>%
    addRasterImage(ras, project = FALSE,
      colors = pal, opacity = 0.8) %>%
    addLegend(values = raster::values(ras),
      title = title, pal = pal)

}

#' Minify a larger aquamaps dataset by slicing
#'
#' Creates a minimized version of a larger AquaMaps database,
#' by filtering all tables to contain a single identifier.
#' @param key the species identifier to keep
#' @param slice_file the file path for storing the resulting SQLite3 db
#' @export
#' @importFrom DBI dbDisconnect
#' @importFrom RSQLite dbConnect SQLite dbDisconnect dbWriteTable
#' @importFrom purrr iwalk
#' @importFrom dplyr tbl filter distinct pull
db_minify <- function(key, slice_file) {

  con <- con_am("sqlite")
  on.exit(DBI::dbDisconnect(con))

  csc <-
    con %>% tbl("hcaf_species_native") %>%
    filter(.data$SpeciesID == key) %>%
    distinct(.data$CsquareCode) %>%
    pull(.data$CsquareCode)

  hcaf_r <-
    con %>% tbl("hcaf_r") %>%
    filter(.data$CsquareCode %in% csc) %>%
    collect()

  tbls <- aquamapsdata::am_meta %>% .data$table %>% unique()

  ft <- function(x)
    con %>% tbl(x) %>%
    filter(.data$SpeciesID == key) %>%
    collect()

  other <- tbls[2:5] %>% map(ft)
  names(other) <- tbls[2:5]

  am_slice <- c(hcaf_r = list(hcaf_r), other)

  con_dest <-
    RSQLite::dbConnect(RSQLite::SQLite(), slice_file)
  on.exit(RSQLite::dbDisconnect(con_dest))

  wt <- function(x, y)
    con_dest %>% RSQLite::dbWriteTable(name = y, value = x)

  am_slice %>% purrr::iwalk(wt)

}


