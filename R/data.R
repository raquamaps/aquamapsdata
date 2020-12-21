#' @importFrom DBI dbDisconnect dbExistsTable dbExecute dbGetQuery
#' @noRd
#' @family admin
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
}

#' Fuzzy search for terms related to taxonomic names
#' @param search_term token query, phrase query or NEAR query
#' (see http://www.sqlite.org/fts5.html)
#' @return tibble with matching keys (database identifiers)
#' @examples \dontrun{
#' am_search_fuzzy("trevally")
#' am_search_fuzzy("trevally OR animalia")
#' }
#' @export
#' @family general
am_search_fuzzy <- function(search_term) {

  query <- paste("select key, terms from fts",
                 sprintf("where terms match '%s'", search_term))

  am_custom_query(query)

}

#' Run a custom SQL query
#' @param sql_query the query
#' @param con the connection to use, if missing an sqlite con is used
#' @param ... other arguments to be passed to the sql() fcn
#' @examples \dontrun{
#' am_custom_query("select * from speciesoccursum_r", con = con_am("extdata"))
#' }
#' @importFrom dplyr sql tbl collect
#' @importFrom DBI dbDisconnect
#' @export
#' @family admin
am_custom_query <- function(sql_query, con, ...) {

  if (missing(con)) {
    con <- db_cache$local_db
  }

  con %>%
    tbl(sql(sql_query, ...)) %>%
    collect()

}

#' @importFrom DBI dbRemoveTable
#' @importFrom purrr map
#' @noRd
#' @family admin
am_drop_fts <- function(con) {

  if (missing(con)) {
    con <- con_am("sqlite")
    on.exit(DBI::dbDisconnect(con))
  }

  fts_shadow_tables <- function(tablename)
    sprintf("%s_%s", tablename,
            c("data", "idx", "config", "content", "docsize"))

  drop_table <- function(x)
    DBI::dbRemoveTable(con, x, fail_if_missing = FALSE)

  fts_tabs <- c("fts", fts_shadow_tables("fts"))

  fts_tabs %>% purrr::map(drop_table)

}

#' Exact search for taxonomic names
#'
#' Search taxonomic names with or without parameters specified.
#' - If parameters are given, the intersection of matching records
#' are returned.
#' - If no parameters are given, all records are returned.
#' - If parameters are specified with NULL specified, examples of
#' valid combinations of values are returned.
#' @param SpeciesID AquaMaps unique identifier for a valid species used by the
#' Catalogue of Life Annual Checklist (www.catalogueoflife.org). Example for
#' the whale shark: Fis-30583
#' @param SpecCode Species identifier used in FishBase or SeaLifeBase
#' @param Genus Genus name of the species
#' @param Species Specific epithet of the species
#' @param FBname Common name suggested by FishBase or SeaLifeBase
#' @param OccurRecs Number of point records used to generate good cells
#' @param OccurCells Number of good cells used to generate species envelope
#' @param StockDefs Distribution of the species as recorded in
#' FishBase or SeaLifeBase
#' @param Kingdom Kingdom to which the species belongs
#' @param Phylum Phylum to which the species belongs
#' @param Class Class to which the species belongs
#' @param Order Order to which the species belongs
#' @param Family Family to which the species belongs
#' @param deepwater Does the species occur in the deep-sea (i.e. tagged
#' bathypelagic or bathydemersal in FishBase or SeaLifeBase)? 0=No, 1=Yes
#' @param angling Is the species a sport fish (i.e. tagged as a GameFish in
#' FishBase)? 0=No, 1=Yes
#' @param diving Is the species found on a dive (i.e. where DepthPrefMin in
#' HSPEN < 20 meters)? 0=No, 1=Yes
#' @param dangerous Is the species dangerous (i.e. tagged as traumatogenic or
#' venonous in FishBase or SeaLifeBase)? 0=No, 1=Yes
#' @param m_invertebrates Is the species a marine invertebrate? 0=No, 1=Yes
#' @param highseas Is the species an open ocean fish species (i.e. tagged as
#' pelagic-oceanic in FishBase)? 0=No, 1=Yes
#' @param invasive Is the species recorded to be invasive (i.e. in FishBase
#' or SeaLifeBase)? 0=No, 1=Yes
#' @param resilience Resilience of the species (i.e. as recorded in
#' FishBase/SeaLifeBase)
#' @param iucn_id IUCN species identifier
#' @param iucn_code IUCN Red list classification assigned to the species
#' @param iucn_version IUCN version
#' @param provider FishBase (FB) or SeaLifeBase (SLB)?
#' @return tibble with results...
#' @examples \dontrun{
#' am_search_exact()
#' am_search_exact(Species = "bucculentus")
#' am_search_exact(FBname = NULL, provider = NULL)
#' }
#' @export
#' @family general
am_search_exact <- function(
  SpeciesID=NULL, SpecCode=NULL, Genus=NULL, Species=NULL, FBname=NULL,
  OccurRecs=NULL, OccurCells=NULL, StockDefs=NULL, Kingdom=NULL, Phylum=NULL,
  Class=NULL, Order=NULL, Family=NULL, deepwater=NULL, angling=NULL,
  diving=NULL, dangerous=NULL, m_invertebrates=NULL, highseas=NULL,
  invasive=NULL, resilience=NULL, iucn_id=NULL, iucn_code=NULL,
  iucn_version=NULL, provider=NULL) {

  args <-  as.list(match.call(expand.dots = TRUE)[-1])

  tc <- function(l) Filter(Negate(is.null), l)
  tcn <- function(l) Filter(is.null, l)

  if (length(tcn(args)) > 0) {
    nullfields <- paste0(collapse = ", ", names(tcn(args)))
    message("Got NULL params for ", nullfields)
    message("Showing some example combinations")

    sql <-
      sprintf(paste0("select distinct * from (select %s ",
                     "from speciesoccursum_r limit 100) limit 10"), nullfields)
    return(am_custom_query(sql))
  }

  if (!(length(tc(args)) > 0))
    return(am_custom_query("select * from speciesoccursum_r"))

  dbq <- function(x)
    DBI::dbQuoteString(DBI::ANSI(),
                       DBI::dbQuoteString(DBI::ANSI(), DBI::SQL(x)))

  sql_where <- function(x)
    sprintf('%s = "%s"', names(x), dbq(x))

  kv <- purrr::map(tc(args), eval)
  sql_where <- paste(collapse = " and ", sql_where(kv))
  sql <- sprintf("select * from speciesoccursum_r where %s", sql_where)
  message("query: ", sql)
  am_custom_query(sql)
}

#' @importFrom DBI dbDisconnect dbExecute
#' @noRd
#' @family admin
am_create_indexes <- function(con) {

  if (missing(con)) {
    con <- con_am("sqlite")
    on.exit(DBI::dbDisconnect(con))
  }

  i <-
    aquamapsdata::am_meta %>%
    filter(.data$field %in% c("SpeciesID", "LOICZID", "CsquareCode")) %>%
    select(.data$table, .data$field)

  idx <- paste0(i$field, seq_len(length(i$field)))

  sql <-
    sprintf("create index if not exists [%s] on %s([%s])",
            idx, i$table, i$field)

  execute <- function(x) {
    res <- DBI::dbExecute(con, x)
    message("Index created: ", res, ", sql: ", x)
    res
  }
  res <- sql %>% map(execute)

  if (!all(res == 0)) message("Done")
}

#' @family general
am_keys <- function() {

  con <- db_cache$local_db

  con %>%
    dplyr::tbl("speciesoccursum_r") %>%
    dplyr::distinct(.data$SpeciesID) %>%
    dplyr::collect() %>%
    dplyr::pull(.data$SpeciesID)
}

#' @importFrom rlang .data
#' @noRd
#' @family general
am_nativemaps <- function(key) {

  if (!all(key %in% am_keys()))
    stop("Please specify valid key(s)")

  con <- db_cache$local_db

  con %>%
    dplyr::tbl("hcaf_species_native") %>%
    dplyr::filter(.data$SpeciesID %in% key) %>%
    dplyr::collect() %>%
    dplyr::select(
      .data$SpeciesID, .data$CsquareCode,
      .data$CenterLat, .data$CenterLong,
      .data$Probability) %>%
    dplyr::rename(lat = "CenterLat", lon = "CenterLong")

}


#' Half degree cell location reference data
#' This table represents an authority file with reference data for half degree
#' cell locations.
#' @examples \dontrun{
#' am_hcaf()
#' }
#' @export
#' @importFrom dplyr tbl
#' @family general
am_hcaf <- function() {

  con <- db_cache$local_db

  con %>%
    dplyr::tbl("hcaf_r")
}

#' Environmental envelope preference data for suitable habitats for species
#' This table provides data on species environmental parameters / preferences
#' @examples \dontrun{
#' keys <- am_search_fuzzy("trevally")$key[1:2]
#' am_hspen() %>% filter(SpeciesID %in% keys)
#' }
#' @export
#' @importFrom dplyr tbl
#' @family general
am_hspen <- function() {

  con <- db_cache$local_db

  con %>%
    dplyr::tbl("hspen_r")
}
