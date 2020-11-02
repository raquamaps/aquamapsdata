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

## ---- fig.show='hold', eval=FALSE---------------------------------------------
#  
#  library(aquamapsdata)
#  library(dplyr)
#  
#  my_db <- aquamapsdata:::src_sqlite_aquamapsdata()
#  
#  my_db %>% tbl("nativemaps")
#  my_db %>% tbl("hcaf")
#  my_db %>% tbl("hspen")
#  my_db %>% tbl("occ")
#  my_db %>% tbl("taxa")
#  

## ---- fig.show='hold', message=FALSE------------------------------------------
library(aquamapsdata)
library(dplyr)

my_db <- aquamapsdata:::src_sqlite_aquamapsdata()

record_count <- 
  my_db %>% tbl("occ") %>% 
  summarize(count = n()) %>% 
  collect %>% 
  .$count

record_count


## ---- fig.show='hold', message=FALSE------------------------------------------

library(tidyr)

# filter one table for a specific record
taxon_wide <- 
  my_db %>% tbl("taxa") %>% 
  filter(SPECIESID == "Fis-26653") %>%
  collect

# pivot the result for easier display  
taxon_tall <- 
  taxon_wide %>% 
  gather(col_name, col_val)

# display
knitr::kable(taxon_tall)


## ---- fig.show='hold', message=FALSE------------------------------------------
library(dplyr)
library(purrr)

# function which gives the record cound for a given table
ls_count <- function(table) {
  res <- table %>% summarize(count = n()) %>% collect %>% .$count
  title <- as.character(table$ops$x)
  tibble(table = title, record_count = res)
}

# get record counts for all tables
am_counts <- bind_rows(
  my_db %>% tbl("nativemaps") %>% ls_count,
  my_db %>% tbl("hcaf") %>% ls_count,
  my_db %>% tbl("hspen") %>% ls_count,
  my_db %>% tbl("occ") %>% ls_count,
  my_db %>% tbl("taxa") %>% ls_count
)

# display
knitr::kable(am_counts)

## ---- fig.show='hold', message=FALSE------------------------------------------

# fuzzy search for "trout OR cod"

keys <- am_name_search_fuzzy("trout OR cod")$key

# exact results for all those keys

hits <- map_df(keys, function(x) am_name_search_exact(key = x))

# we inspect the species list we got 

display <- hits %>% select(key, binomial, rank_family, vernacular)
knitr::kable(display)


## ---- fig.show='hold', message=FALSE------------------------------------------
library(DT)

# for a db table, return a tibble with the columns and their data types
ls_types <- function(table) {
  res <- table %>% head %>% collect %>% lapply(type_sum) %>% unlist
  colname <- names(res)
  title <- as.character(table$ops$x)
  tibble(table = title, col_name = colname, col_type = res, desc = NA)
}

# run the above function on all tables
am_schema <- bind_rows(
  my_db %>% tbl("nativemaps") %>% ls_types,
  my_db %>% tbl("hcaf") %>% ls_types,
  my_db %>% tbl("hspen") %>% ls_types,
  my_db %>% tbl("occ") %>% ls_types,
  my_db %>% tbl("taxa") %>% ls_types
)

datatable(am_schema)


## ---- fig.show='hold', message=FALSE------------------------------------------

duplicated_colnames <- 
  unique(am_schema$col_name[duplicated(am_schema$col_name)])

am_keys <- 
  am_schema %>% 
  filter(col_name %in% duplicated_colnames) %>% 
  arrange(col_name)

# sometimes the datatypes are different where the column name are equal

knitr::kable(am_keys)


## ---- fig.show='hold', eval=FALSE, message=FALSE------------------------------
#  
#  readr::write_csv(am_schema, path = "~/am-schema.csv")
#  readr::write_csv(am_keys, path = "~/am-keys.csv")
#  

