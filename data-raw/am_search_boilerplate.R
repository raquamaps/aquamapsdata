am_search_boilerplate <- function() {

  am_params <-
    am_meta %>% filter(table == "speciesoccursum_r") %>%
    select(field, description)

  clean <- function(x) gsub("'", "", x)
  doc <- function(am_params) {
    sprintf("#' @param %s %s", am_params$field, clean(am_params$description))
  }

  sig <- function(am_params) {
    sprintf("%s=NULL", am_params$field)
  }

  template <- sprintf(
"#' Exact search for taxonomic names
%s
#' @return tibble with matching identifiers...
am_search_exact <- function(
  %s){

}
",
    paste(collapse = "\n", doc(am_params)),
    paste(sig(am_params), collapse = ", ")
  )

  template
}

clipr::write_clip(readr::read_lines(am_search_boilerplate()))


con <- con_am("sqlite")
on.exit(dbDisconnect(con))

taxa <-
  con %>% tbl("speciesoccursum_r") %>% collect %>%
  mutate(binomen = paste(Genus, Species))

unique_values <- function(param, col_name) {
  message("returning unique values for argument/parameter ", param)
  res <- taxa %>% select(!!col_name) %>%
    distinct %>% collect %>%
    select(1)
  names(res) <- param
  return (res)
}
