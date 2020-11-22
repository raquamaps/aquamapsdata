## ---- eval=FALSE--------------------------------------------------------------
#  
#  # install aquamapsdata from GitHub using devtools
#  
#  install.packages("devtools")
#  library("devtools")
#  install_gitub("raquamaps/aquamapsdata")
#  
#  # initial run-once step required to install remote db locally
#  
#  library(aquamapsdata)
#  download_db(force = TRUE)
#  

## ---- message=FALSE-----------------------------------------------------------
library(aquamapsdata)
library(dplyr)

# fuzzy search allows full text search operators AND, OR, NOT and +
# see https://www.sqlitetutorial.net/sqlite-full-text-search/
am_search_fuzzy("atlantic cod OR elephantfish")$key

# exact search without parameters returns all results
nrow(am_search_exact())

# exact search giving NULL params shows examples of existing values
# here we see what combinations are present in the dataset for 
# angling, diving, dangerous, highseas, deepwater organisms
am_search_exact(
  angling = NULL, diving = NULL, dangerous = NULL, 
  deepwater = NULL, highseas = NULL, m_invertebrates = NULL)

# exact search without NULL params, specifying values
hits <- am_search_exact(angling = 1, diving = 1, dangerous = 1, Family = "Callorhinchidae")

# display results
display <- 
  hits %>% mutate(binomen = paste(Genus, Species)) %>%
  select(SpeciesID, binomen, SpecCode, FBname)

knitr::kable(display)


## ----"map", echo=TRUE, fig.width=7, message=FALSE-----------------------------

library(aquamapsdata)
library(leaflet)
library(raster)

# get the identifier for atlantic cod
key <- am_search_fuzzy("atlantic cod")$key

# show the native habitat map
am_map_leaflet(key)

## ---- echo=TRUE, fig.width=7, message=FALSE-----------------------------------

keys <- am_search_exact(Genus = "Clupea")$SpeciesID
am_map_leaflet(keys)


## ---- message=FALSE-----------------------------------------------------------

con <- aquamapsdata:::con_am("sqlite")
aquamapsdata:::db_counts(con)
DBI::dbDisconnect(con)

## -----------------------------------------------------------------------------
knitr::kable(am_meta)

