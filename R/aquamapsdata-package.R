#' aquamapsdata
#'
#' This package provides data access to curated data from aquamaps.org.
#'
#' A local SQLite db is downloaded from
#' https://archive.org/download/aquamapsdb
#'
#' This database is then exposed in this package for querying (using dplyr)
#'
#' @import dplyr RSQLite
#' @name aquamapsdata-package
#' @aliases aquamapsdata
#' @keywords package
#' @family meta
NULL

#' Metadata describing field names available
#'
#' A dataset with field names and field descriptions for the included tables.
#'
#' The metadata is provided as a data frame with the table name, field name,
#' field description and original data type in the AquaMaps.org source db.
#'
#' There are five tables:
#' - **speciesoccursum_r**: This table provides general information about taxa,
#' such as identifiers and taxonomy for mapped species, and allows for
#' searching. This is a simplified version, excluding fields in the source
#' database which are used for internal purposes. *NB*: To be updated when
#' invasive, resilience and IUCN fields are filled and m_invertebrates field
#' is updated.
#' - **hcaf_species_native**: This table stores native habitat map data;
#' usually referred to as HSPEC-native. This is a simplified version,
#' providing only overall probability of occurrence of a species in a cell.
#' Contact the AquaMaps team if you are interested in the individual
#' probabilities of occurrence (depth, temperature, salinity,
#' primary production, sea ice concentration, bottom oxygen, distance to land).
#' *NB*: May be updated in the future with a version that includes records
#' where Probability=0 to explicitly show cells where species is not
#' predicted to occur within its known native distribution.
#' - **hspen_r**: This table provides the parameters used in maps from AquaMaps,
#' such as the **sp**ecies environmental **en**velope for each mapped species.
#' The data is relevant mostly for single species maps.
#' - **hcaf_r**: This **a**uthority**f**ile describes some attributes for 0.5
#' degree raster grid cells with species presence in the data. The data in this
#' table excludes future environmental parameter fields. Contact the AquaMaps
#' team if you are interested in the future environmental parameters.
#' Available datasets correspond to IPCC RCP 2.6 (2100),
#' RCP 4.5 (2050 and 2100), RCP 8.5 (2050 and 2100).
#' - **occurrencecells_r**: This table provides the occurrence cells generated
#' from assembled occurrence data and it is used to generate the species
#' envelope for each mapped species. *NB*: Fields where null=No should be
#' updated to 0=No.
#' @format A data frame:
#' \describe{
#'   \item{table}{table name}
#'   \item{field}{field or column name}
#'   \item{description}{explanation of the field content}
#'   \item{type_mysql}{original field type in the source database}
#'   \item{type}{field type when used from R}
#' }
#' @source [AquaMaps](https://aquamaps.org/)
#' @family meta
"am_meta"

#' Citation to use when publishing data from this source
#'
#' You are welcome to include text, numbers and maps from AquaMaps in your
#' own work for non-commercial use, given that such inserts are clearly
#' identified as coming from AquaMaps.org, with a backward link to the
#' respective source.
#'
#' A researcher planning publication based on these datasets is invited
#' to contact the AquaMaps team with queries related to specific content.
#' The AquaMaps team can help with double checking for correctness
#' prior to drawing conclusions and/or subsequent publication,
#' and provide further clarification of update frequency,
#' limitations  and/or interpretation of unlikely results.
#'
#' Please open an issue on the
#' [GitHub issue tracker](https://github.com/raquamaps/aquamapsdata/issues) or
#' contact the team directly by email at Rainer Froese (rfroese (at) geomar.de)
#' or Kristin Kaschner (Kristin.Kaschner (at) biologie.uni-freiburg.de).
#' @param format one of "text" or "md", default: "text
#' @return citation text and copyright info
#' @export
#' @family meta
am_citation <- function(format = c("text", "md")) {

  citation <-
    paste0("Kaschner, K., K. Kesner-Reyes, C. Garilao, J. Segschneider, ",
         "J. Rius-Barile, T. Rees, and R. Froese. 2019. ",
         "AquaMaps: Predicted range maps for aquatic species. ",
         "World wide web electronic publication, www.aquamaps.org, ",
         "Version 10/2019.")

  copyright_md <-
    paste0("Content from AquaMaps as provided in this R package is ",
    "licensed under a [Creative Commons Attribution-NonCommercial 3.0 Unported",
    "License](http://creativecommons.org/licenses/by-nc/3.0/): ",
    "![](https://i.creativecommons.org/l/by-nc/3.0/80x15.png)",
    "{style='border-width:0'}")

  copyright_text <-
    paste0("Content from AquaMaps as provided in this R package is ",
    "licensed under a Creative Commons Attribution-NonCommercial 3.0 Unported",
    "License, please see http://creativecommons.org/licenses/by-nc/3.0/")

  switch(match.arg(format),
   "text" = paste0(collapse = "\n", c(citation, copyright_text)),
   "md" = paste0(collapse = "\n\n", c(citation, copyright_md))
  )
}
