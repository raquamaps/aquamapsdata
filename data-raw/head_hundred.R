library(dplyr)

old_db <- aquamapsdata:::aquamapsdata_sqlite()

my_db <- RSQLite::dbConnect(RSQLite::SQLite(),
  "inst/extdata/am.db")

hundred <- function(tab) old_db %>%
  tbl(tab) %>% head(100) %>% collect

taxa <- old_db %>% tbl("taxa") %>% collect

copy_to(my_db, hundred("nativemaps"), "nativemaps",
        temporary = FALSE, overwrite = TRUE,
        indexes = list(c("SpeciesID")))

copy_to(my_db, hundred("hcaf"), "hcaf",
        temporary = FALSE, overwrite = TRUE,
        indexes = list(c("LOICZID")))

copy_to(my_db, hundred("hspen"), "hspen",
        temporary = FALSE, overwrite = TRUE,
        indexes = list(c("SpeciesID")))

copy_to(my_db, hundred("occ"), "occ",
        temporary = FALSE, overwrite = TRUE,
        indexes = list(c("SpeciesID")))

copy_to(my_db, taxa, "taxa",
        temporary = FALSE, overwrite = TRUE,
        indexes = list(c("SPECIESID")))

con <- my_db

library(DBI)

if (!"fts" %in% dbListTables(con)) {
  dbSendQuery(con, statement =
                "create virtual table fts using fts4(
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

dbDisconnect(con)
