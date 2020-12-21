library(readxl)
library(here)
library(purrr)
library(dplyr)
library(pillar)

# read metadata about tables from Excel file
file_meta <- file.path(here(), "data-raw", "AquaMaps_DBStructure_R_9Oct20.xlsx")
sheets <- excel_sheets(file_meta)
n_skip <- c(7, 5, 7, 6, 5)
tbl_names <- paste0(sheets, "_r")
tbl_names <- recode(tbl_names, "hcaf_species_native_r" = "hcaf_species_native")

meta <-
  map2(sheets, n_skip, function(x, y) read_xlsx(file_meta, sheet = x, skip = y))

am_meta <-
  map2(meta, tbl_names, function(x, y)
  x %>% mutate(Table = y, Size = as.character(Size)) %>%
    rename_with(Name = "Field", DataType = "Type", .fn = recode)) %>%
  bind_rows() %>%
  select(Table, Field, Description, Type) %>%
  rename_with(.fn = tolower)

usethis::use_data(am_meta, overwrite = TRUE)

# compare to existing fields in local database
# requires a db_sync() from mysql/mariadb to sqlite prior to this

sqlite <- con_am("sqlite")

compare_fields <- function(table)
  sqlite %>% DBI::dbListFields(table) %>% tibble(localdb = table, field = .) %>%
  left_join(am_meta, by = "field") %>% filter(is.na(description))

tbl_names %>% map_df(compare_fields)

# summarize the dbschema across tables in local database
am_dbschema <- function() {

  sqlite <- con_am("sqlite")
  on.exit(DBI::dbDisconnect(sqlite))

  # for a db table, return a tibble with the columns and their data types
  ls_types <- function(table) {
    res <- table %>% head %>% collect %>% lapply(pillar::type_sum) %>% unlist
    colname <- names(res)
    title <- as.character(table$ops$x)
    tibble(table = title, field = colname, type_local = res)
  }

  tbls <- unique(am_meta$table)
  lst <- function(x) sqlite %>% tbl(x) %>% ls_types

  tbls %>% purrr::map(lst) %>% bind_rows() %>%
    left_join(am_meta) %>%
    select(table, field, description, type_mysql = type, type = type_local)
}

am_meta <-
  am_dbschema()

usethis::use_data(am_meta, overwrite = TRUE)

am_meta

dupes <- with(am_meta, field[duplicated(tolower(field))])
am_meta %>% filter(field %in% dupes) %>% arrange(field) %>% filter(grepl("*pec*", field))

###########



am_meta %>% filter(grepl("*ode", field))

# this one seems to be spelled Slimit in db, instead of SLimit as in metadata spec
sqlite %>% DBI::dbListFields("hcaf_r") %>% grep("*limit", ., value = TRUE)

# is this one missing from the metadata?
sqlite %>% DBI::dbListFields("hspen_r") %>% grep("code", ., value = TRUE)

# yes -> make corrections in Excel-file

am_meta <-
  am_meta %>% mutate(field = recode(field, SpecCode = "Speccode", SLimit = "Slimit"))

usethis::use_data(am_meta, overwrite = TRUE)

tbl_names %>% map_df(compare_fields)

# aha, in those tables the field is actually called "SpeciesCode", fix this...
sqlite %>% DBI::dbListFields("speciesoccursum_r") %>% grep("ode", ., value = TRUE)
sqlite %>% DBI::dbListFields("occurrencecells_r") %>% grep("ode", ., value = TRUE)

am_meta <-
  am_meta %>%
  mutate(field = ifelse(table %in% c("speciesoccursum_r", "occurrencecells_r"), "SpecCode", field))

usethis::use_data(am_meta, overwrite = TRUE)

tbl_names %>% map_df(compare_fields)
