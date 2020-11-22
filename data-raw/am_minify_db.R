library(dplyr)

# This script minifies a larger database and embeds it into the package

# con_am("sqlite") %>% tbl("hcaf_species_native") %>% group_by(SpeciesID) %>%
#  count(SpeciesID) %>% arrange(n) %>% filter(n > 1000) %>% head(5)

#am_search_exact(SpeciesID = "Fis-29757") %>% View()

am_search_fuzzy("Bluespotted trevally")$key %>%
  db_minify("inst/extdata/am.db")

db_min <- RSQLite::dbConnect(RSQLite::SQLite(),
  "inst/extdata/am.db")

#am_create_indexes(db_min)
am_create_fts(db_min)

dbDisconnect(db_min)
