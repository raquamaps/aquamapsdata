library(aquamapsdata)

context("Name Search")

test_that("fuzzy search works with OR", {
  with_OR <- am_name_search_fuzzy(search_term = "Cod OR shark")
  without_OR <- am_name_search_fuzzy(search_term = "Cod")
  expect_gt(nrow(with_OR), nrow(without_OR))
})


test_that("custom query works", {
  res <- am_sql_query("select key from fts where terms match 'cod'")
  expect_gt(nrow(res), 0)
})

# am_name_search_exact()
# am_name_search_exact(key = NULL)
# am_name_search_exact(key = "Biv-32126")
#
# am_name_search_exact(binomial = NULL)
# am_name_search_exact(binomial = "Oreochromis niloticus")
# am_name_search_exact(binomial = "Salmo trutta")
# am_name_search_exact(binomial = "Gadus morhua")
#
# am_name_search_exact(vernacular = NULL)
# am_name_search_exact(vernacular = "Atlantic cod")
#
# am_name_search_exact(rank_genus = NULL)
# am_name_search_exact(rank_genus = "Alosa")
# am_name_search_exact(rank_genus = "Alosa", rank_species = "fallax")
#
# am_name_search_exact(rank_kingdom = NULL)
# am_name_search_exact(rank_kingdom = "Animalia")
#
# am_name_search_exact(rank_phylum = NULL)
# am_name_search_exact(rank_phylum = "Annelida")
#
# am_name_search_exact(rank_class = NULL)
# am_name_search_exact(rank_class = "Mammalia")
#
# am_name_search_exact(rank_order = NULL)
#
# am_name_search_exact(rank_genus = NULL)
# am_name_search_exact(rank_species = NULL)
