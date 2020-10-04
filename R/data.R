#' SQLite database connection to the aquamaps data
#' @importFrom dplyr src_sqlite
#' @importFrom RSQLite dbConnect
#' @noRd
src_sqlite_aquamapsdata <- function() {

  AM_DB <- am_db_path()

  if (!file.exists(AM_DB))
    stop("No database available at ", AM_DB,
         " please use download_db()")

  if (file.size(AM_DB) == 0) {
    message("Removing emtpy db at ", AM_DB)
    unlink(AM_DB)
    stop("Database empty, aborting, please use download_db()")
  }

  my_db <- RSQLite::dbConnect(RSQLite::SQLite(), AM_DB)
  #my_db <- dplyr::src_sqlite(AM_DB)
  return (my_db)
}

#' @noRd
am_db_path <- function() {
  file.path(system.file(package = "aquamapsdata"),
    "extdata", "am.db")
}

#' Run a custom SQL query
#' @param sql_query the query
#' @param ... other arguments to be passed to the sql() fcn
#' @importFrom dplyr sql tbl collect
#' @importFrom RSQLite dbDisconnect
#' @export
am_sql_query <- function(sql_query, ...){

  con <- src_sqlite_aquamapsdata()

  res <-  con %>%
    tbl(sql(sql_query, ...)) %>%
    collect()

  on.exit(dbDisconnect(con))

  return (res)
}

#' @noRd
add_fts_table <- function() {

  con <- src_sqlite_aquamapsdata()

  if (!"fts" %in% dbListTables(con)) {
    dbSendQuery(con, statement =
    "create virtual table fts using fts5(
      key, terms
    );")

    dbSendQuery(con, statement = paste0(
    "insert into fts select SPECIESID as key, printf('",
    paste(collapse = " ", rep("%s", 8)), "',
    `Genus`, `Species`, `FBname`,
    `Kingdom`, `Phylum`, `Class`,
    `Order`, `Family`
    ) as terms FROM taxa;"))
  }

  on.exit(dbDisconnect(con))

  # res <- am_name_search() %>% collect %>%
  #   tidyr::unite(terms, -key, sep = " ") %>%
  #   select(key, terms)
  #
  # RSQLite::dbWriteTable(src_sqlite_aquamapsdata(),
  #  "fts", res, overwrite = FALSE, row.names = FALSE)

}

#' Fuzzy search for terms related to taxonomic names
#' @param search_term token query, phrase query or NEAR query (see http://www.sqlite.org/fts3.html)
#' @return tibble with matching keys (database identifiers)
#' @examples
#'  am_name_search_fuzzy("cod")
#'  am_name_search_fuzzy("cod OR shark")
#' @export
am_name_search_fuzzy <- function(search_term) {
  query <- paste("select key from fts",
    sprintf("where terms match '%s'", search_term))
  res <- am_sql_query(query)
  #return(as_tibble(res))
  return (res)
}

#' Exact search for taxonomic names
#' @param key a database identifier string
#' @param binomial a string on the form Genus Species
#' @param vernacular a string with a popular name
#' @param rank_genus a string with the genus name
#' @param rank_species a string with the species name
#' @param rank_family a string with the family name
#' @param rank_kingdom a string with the kingdom name
#' @param rank_phylum a string with the phylum name
#' @param rank_class a string with the class name
#' @param rank_order a string with the order name
#' @return tibble with matching identifiers...
#' #' If the function is called with only one argument and NULL as the value, the result will list all unique values that are available
#' If the function is called with one or more arguments with real values a tibble will be return with keys or identifiers which exactly matches the search
#' @examples
#'  am_name_search_exact(vernacular = "Atlantic cod")
#'  am_name_search_exact(key = "Fis-23638")
#' @export
am_name_search_exact <- function(
  key = NULL, binomial = NULL, vernacular = NULL,
  rank_genus = NULL, rank_species = NULL,
  rank_family = NULL, rank_kingdom = NULL, rank_phylum = NULL,
  rank_class = NULL, rank_order = NULL) {

  con <- src_sqlite_aquamapsdata()
  on.exit(dbDisconnect(con))

  taxa <- con %>% tbl("taxa") %>% collect %>% mutate(binomen = paste(Genus, Species))

  unique_values <- function(param, col_name) {
    message("returning unique values for argument/parameter ", param)
    res <- taxa %>% select(!!col_name) %>%
      distinct %>% collect %>%
      select(1)
    names(res) <- param
    return (res)
  }

  if (!missing(key) && is.null(key))
    return (unique_values("key", "SPECIESID"))

  if (!missing(binomial) && is.null(binomial)) {
    message("returning distinct binomial (Genus, Species) values")
    res <- taxa %>% select(binomen) %>% distinct() %>% collect %>% select(binomen)
#      select(Genus, Species) %>%
#      distinct %>% collect %>%
#      mutate(binomial = paste(Genus, Species)) %>%
#      select(binomial)
    return (res)
  }

  if (!missing(vernacular) && is.null(vernacular))
    return(unique_values("vernacular", "FBname"))

  if (!missing(rank_genus) && is.null(rank_genus))
    return(unique_values("rank_genus", "Genus"))

  if (!missing(rank_species) && is.null(rank_species))
    return(unique_values("rank_species", "Species"))

  if (!missing(rank_family) && is.null(rank_family))
    return(unique_values("rank_family", "Family"))

  if (!missing(rank_class) && is.null(rank_class))
    return(unique_values("rank_class", "Class"))

  if (!missing(rank_order) && is.null(rank_order))
    return(unique_values("rank_order", "Order"))

  if (!missing(rank_kingdom) && is.null(rank_kingdom))
    return(unique_values("rank_kingdom", "Kingdom"))

  if (!missing(rank_phylum) && is.null(rank_phylum))
    return(unique_values("rank_phylum", "Phylum"))

  res <- taxa


  if (!is.null(key)) res <- res %>%
    filter(SPECIESID == key)

  if (!is.null(binomial)) {
    res <- res %>%
      filter(binomen == binomial)
  }

  if (!is.null(vernacular)) res <- res %>%
    filter(FBname == vernacular)

  if (!is.null(rank_genus)) res <- res %>%
    filter(Genus == rank_genus)

  if (!is.null(rank_species)) res <- res %>%
    filter(Species == rank_species)

  if (!is.null(rank_family)) res <- res %>%
    filter(`Family` == rank_family)

  if (!is.null(rank_order)) res <- res %>%
    filter(`Order` == rank_order)

  if (!is.null(rank_phylum)) res <- res %>%
    filter(Phylum == rank_phylum)

  if (!is.null(rank_class)) res <- res %>%
    filter(Class == rank_class)

  if (!is.null(rank_kingdom)) res <- res %>%
    filter(Kingdom == rank_kingdom)

#see https://stackoverflow.com/questions/42123011/sqlite-row-value-misused-error-in-sqlite
#   SELECT *
# FROM `taxa`
# WHERE (`Genus` = CASE WHEN (1.0) THEN (('Salmo', 'trutta')) END AND `Species` = CASE WHEN (2.0) THEN (('Salmo', 'trutta')) END)

  res <- res %>% collect() %>%
    select(key = SPECIESID, binomial = binomen,
           vernacular = FBname,
           rank_genus = Genus, rank_species = Species,
           rank_kingdom = Kingdom, rank_phylum = Phylum,
           rank_class = Class,
           rank_order = `Order`, rank_family = `Family`)
  return (res)
}

#' @importFrom utils globalVariables
if (getRversion() >= "2.15.1")
  globalVariables(names = unlist(strsplit(split = " ",
 paste0("Class FBname Family Genus Kingdom Order ",
  "Phylum SPECIESID Species binomen"))))


# taxa() %>%
#   group_by(Class, `Order`, `Family`) %>%
#   summarize(count = n()) %>%
#   collect %>%
#   arrange(desc(count), Class, `Order`)
