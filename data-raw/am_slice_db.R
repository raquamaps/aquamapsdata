library(dplyr)

# these are species which have known "good occurrences" in 10 or more cells
keys <-
  db_cache$local_db %>% tbl("speciesoccursum_r") %>% filter(OccurCells >= 10) %>%
  pull(SpeciesID) %>% unique()

db_minify(keys, "/tmp/am.db")

con_slice <- RSQLite::dbConnect(RSQLite::SQLite(), "/tmp/am.db")

# create indexes (regular and FTS)
am_create_indexes(con_slice)
am_create_fts(con_slice)

dbDisconnect(con_slice)

R.utils::bzip2("/tmp/am.db")
