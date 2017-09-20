## ---- eval=FALSE---------------------------------------------------------
#  
#  # install aquamapsdata from GitHub using devtools
#  install.packages("devtools")
#  library("devtools")
#  
#  install_git("https://github.com/raquamaps/aquamapsdata.git",
#              build_vignettes=FALSE)
#  
#  library(aquamapsdata)
#  
#  # run-once action to install remote db locally
#  download_db()
#  

## ---- fig.show='hold', eval=FALSE----------------------------------------
#  library(aquamapsdata)
#  
#  nativemaps()
#  hcaf()
#  hspen()
#  occ()
#  taxa()
#  

## ---- fig.show='hold', message=FALSE-------------------------------------
library(aquamapsdata)
library(dplyr)

record_count <- 
  occ() %>% 
  summarize(count = n()) %>% 
  collect %>% 
  .$count

record_count


## ---- fig.show='hold', message=FALSE-------------------------------------

library(tidyr)

# filter one table for a specific record
taxon_wide <- 
  taxa() %>% 
  filter(SPECIESID == "Fis-26653") %>%
  collect

# pivot the result for easier display  
taxon_tall <- 
  taxon_wide %>% 
  gather(col_name, col_val)

# display
knitr::kable(taxon_tall)


## ---- fig.show='hold', message=FALSE-------------------------------------
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
  nativemaps() %>% ls_count,
  hcaf() %>% ls_count,
  hspen() %>% ls_count,
  occ() %>% ls_count,
  taxa() %>% ls_count
)

# display
knitr::kable(am_counts)

## ---- fig.show='hold', message=FALSE-------------------------------------
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
  nativemaps() %>% ls_types,
  hcaf() %>% ls_types,
  hspen() %>% ls_types,
  occ() %>% ls_types,
  taxa() %>% ls_types
)

datatable(am_schema)


## ---- fig.show='hold', message=FALSE-------------------------------------

duplicated_colnames <- 
  unique(am_schema$col_name[duplicated(am_schema$col_name)])

am_keys <- 
  am_schema %>% 
  filter(col_name %in% duplicated_colnames) %>% 
  arrange(col_name)

# sometimes the datatypes are different where the column name are equal

knitr::kable(am_keys)


## ---- fig.show='hold', eval=FALSE, message=FALSE-------------------------
#  
#  readr::write_csv(am_schema, path = "~/am-schema.csv")
#  readr::write_csv(am_keys, path = "~/am-keys.csv")
#  

