library(aquamapsdata)

default_db("extdata")

test_that("am_keys returns a non-empty character vector", {
  keys <- am_keys()
  expect_type(keys, "character")
  expect_gt(length(keys), 0)
})

test_that("am_hcaf returns a tbl with expected columns", {
  hcaf <- am_hcaf()
  expect_true(inherits(hcaf, "tbl"))
  cols <- colnames(hcaf)
  expect_true("CsquareCode" %in% cols)
  expect_true("CenterLat" %in% cols)
  expect_true("CenterLong" %in% cols)
})

test_that("am_hspen returns a tbl with expected columns", {
  hspen <- am_hspen()
  expect_true(inherits(hspen, "tbl"))
  cols <- colnames(hspen)
  expect_true("SpeciesID" %in% cols)
})

test_that("am_csc_from_extent returns CsquareCodes within bounds", {
  result <- am_csc_from_extent(100, 120, -22, -7)
  expect_true(is.data.frame(result))
  expect_true("CsquareCode" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("am_csc_from_extent rejects invalid bounds", {
  expect_error(am_csc_from_extent(120, 100, -7, -22))
})

test_that("am_species_in_csc returns species for known cell", {
  csc <- am_csc_from_extent(100, 120, -22, -7)$CsquareCode
  result <- am_species_in_csc(csc, min_prob = 0.5)
  expect_true(is.data.frame(result))
  expect_true("SpeciesID" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("am_species_per_csc returns richness data for known cells", {
  csc <- am_csc_from_extent(100, 120, -22, -7)$CsquareCode
  result <- am_species_per_csc(csc, min_prob = 0.5)
  expect_true(is.data.frame(result))
  expect_true("n_species" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("download_db stops if db exists and force is FALSE", {
  # am_db_sqlite() path doesn't exist in test env, but if it does,
  # force = FALSE should stop with a message about the existing file
  tgt <- am_db_sqlite()
  if (file.exists(tgt)) {
    expect_error(download_db(force = FALSE), "existing db exists")
  } else {
    succeed("no local db present, skipping force=FALSE check")
  }
})

test_that("am_citation returns non-empty strings for both formats", {
  txt <- am_citation("text")
  md  <- am_citation("md")
  expect_type(txt, "character")
  expect_type(md, "character")
  expect_true(nchar(txt) > 0)
  expect_true(nchar(md) > 0)
  expect_true(grepl("AquaMaps", txt))
})
