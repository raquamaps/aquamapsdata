library(aquamapsdata)

context("Name Search")

test_that("fuzzy name search works with OR", {
  with_OR <- am_search_fuzzy(search_term = "Cod OR shark")
  without_OR <- am_search_fuzzy(search_term = "Cod")
  expect_gt(nrow(with_OR), nrow(without_OR))
})


#test_that("custom query works", {
#  res <- am_sql_query("select key from fts where terms match 'cod'")
#  expect_gt(nrow(res), 0)
#})

test_that("exact name search works", {

  expect_gt(nrow(am_search_exact()), 0)

  expect_gt(
    nrow(am_search_exact(Phylum = "Annelida")),
    0
  )

  expect_gt(
    nrow(am_search_exact(Class = "Mammalia")),
    0
  )

  expect_gt(
    nrow(am_search_exact(Species = NULL)),
    0
  )

#  expect_equal(
#    nrow(am_search_exact(SpeciesID = "Biv-32126")),
#    1
#  )

  expect_equal(
    nrow(am_search_exact(Kingdom = NULL)),
    1
  )

  expect_equal(
    nrow(am_search_exact(FBname = "Atlantic cod")),
    1
  )

  expect_gt(
    nrow(am_search_exact(Genus = "Alosa")),
    nrow(am_search_exact(Genus = "Alosa", Species = "fallax"))
  )


})

# test_that("exact name search works for binomial names", {
#
#   expect_equal(
#     am_name_search_exact(binomial = "Salmo trutta")$key,
#     "Fis-21630"
#   )
#
#   expect_equal(
#     am_name_search_exact(binomial = "Gadus morhua")$key,
#     "Fis-29394"
#   )
#
# })
