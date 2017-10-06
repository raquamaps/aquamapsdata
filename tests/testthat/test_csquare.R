library(aquamapsdata)

context("Csquare Conversions")

test_that("csquare conversion from digital degrees work", {

  expect_equal(
    csquare_from_dd(38.8894, -77.0356),
    "7307:487:380:383:495:246"
  )

  expect_equal(
    csquare_from_dd(50.93578, -114.01435, resolution = 0.00001),
    "7511:104:390:131:354:373:485"
  )

  expect_equal(
    csquare_from_dd(-34.57612, -58.43226, resolution = 0.00001),
    "5305:248:354:373:362:112:226"
  )

  expect_equal(
    csquare_from_dd(-42.80000, 147.30000, resolution = 0.00001),
    "3414:227:383:100:100:100:100"
  )

})

test_that("csquare conversion to digital degrees work", {

  expect_equal(
    csquare_to_dd("7307:487:380:383")$initial,
    "NW"
  )

  expect_equal(
    csquare_to_dd("7307:487:380:383")$lat,
    38.88
  )

  expect_equal(
    csquare_to_dd("7307:487:380:383")$lon,
    77.03
  )

  expect_equal(
    csquare_to_dd("7307:487:380:383")$resolution,
    0.001
  )

  expect_equal(
    csquare_to_dd("7307:487:380:383:4")$initial,
    "NW"
  )

  expect_equal(
    csquare_to_dd("7307:487:380:383:4")$lat,
    38.885
  )

  expect_equal(
    csquare_to_dd("7307:487:380:383:4")$lon,
    77.035
  )

  expect_equal(
    csquare_to_dd("7307:487:380:383:4")$resolution,
    0.005
  )

})
