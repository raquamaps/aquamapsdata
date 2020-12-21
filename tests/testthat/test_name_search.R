library(aquamapsdata)

context("Name Search")

default_db("extdata")

test_that("fuzzy name search works with OR", {
  with_OR <- am_search_fuzzy(search_term = "Bluespotted OR trevally")
  without_OR <- am_search_fuzzy(search_term = "Bluespotted")
  is_valid <- nrow(with_OR) >= nrow(without_OR)
  expect_true(is_valid)
})


test_that("custom query works", {
  res <- am_custom_query("select key from fts where terms match 'trevally'")
  expect_gt(nrow(res), 0)
})

test_that("exact name search works", {

  expect_gt(nrow(am_search_exact()), 0)

  expect_gt(
    nrow(am_search_exact(Kingdom = "Animalia")),
    0
  )

  expect_gt(
    nrow(am_search_exact(Class = "Actinopterygii")),
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
    nrow(am_search_exact(FBname = "Bluespotted trevally")),
    1
  )

  expect_true(
    nrow(am_search_exact(Genus = "Caranx")) >=
    nrow(am_search_exact(Genus = "Caranx", Species = "bucculentus"))
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
