#' aquamapsdata
#'
#' This package provides data access to curated data from aquamaps.org.
#'
#' A local SQLite db is created based on raw data files from https://ftp.geomar.de/users/cgarilao/AquaMaps_ver0816.zip
#'
#' This database is then exposed in this package for querying (using dplyr)
#'
#' @import dplyr RSQLite
#' @name aquamapsdata-package
#' @aliases aquamapsdata
#' @keywords package
NULL

#' Metadata describing field names available
#'
#' A dataset with field names and field descriptions for the included tables.
#'
#' The metadata is provided as a data frame with the table name, field name,
#' field description and original data type in the AquaMaps.org source db.
#'
#' There are five tables:
#'
#' - **speciesoccursum_r**: This table provides general information about taxa,
#' such as identifiers and taxonomy for mapped species, and allows for searching.
#' This is a simplified version, excluding fields in the source database which
#' are used for internal purposes. *NB*: To be updated when invasive,
#' resilience and IUCN fields are filled and m_invertebrates field is updated.
#'
#' - **hcaf_species_native**: This table stores native habitat map data;
#' usually referred to as HSPEC-native. This is a simplified version,
#' providing only overall probability of occurrence of a species in a cell.
#' Contact the AquaMaps team if you are interested in the individual
#' probabilities of occurrence (depth, temperature, salinity,
#' primary production, sea ice concentration, bottom oxygen, distance to land).
#' *NB*: May be updated in the future with a version that includes records
#' where Probability=0 to explicitly show cells where species is not
#' predicted to occur within its known native distribution.
#'
#' - **hspen_r**: This table provides the parameters used in maps from AquaMaps,
#' such as the **sp**ecies environmental **en**velope for each mapped species. The data
#' is relevant mostly for single species maps.
#'
#' - **hcaf_r**: This **a**uthority**f**ile describes some attributes for 0.5
#' degree raster grid cells with species presence in the data. The data in this
#' table excludes future environmental parameter fields. Contact the AquaMaps
#' team if you are interested in the future environmental parameters.
#' Available datasets correspond to IPCC RCP 2.6 (2100),
#' RCP 4.5 (2050 and 2100), RCP 8.5 (2050 and 2100).
#'
#' - **occurrencecells_r**: This table provides the occurrence cells generated
#' from assembled occurrence data and it is used to generate the species envelope
#' for each mapped species. *NB*: Fields where null=No should be updated
#' to 0=No.
#'
#' @format A data frame:
#' \describe{
#'   \item{table}{table name}
#'   \item{field}{field or column name}
#'   \item{description}{explanation of the field content}
#'   \item{type_mysql}{original field type in the source database}
#'   \item{type}{field type when used from R}
#' }
#' @source <https://aquamaps.org/>
"am_meta"
