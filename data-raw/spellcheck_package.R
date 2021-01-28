library(spelling)
library(urlchecker)

fix_spelling <- function(words_to_exclude, words_to_include) {

  sc <- spelling::spell_check_package()
  words <- dplyr::tibble(word = sc$word) %>% dplyr::mutate(id = 1:length(word))
  refs <- setNames(sc$found, 1:length(sc$found))
  loc <- dplyr::tibble(loc = unlist(refs), id = as.integer(names(unlist(refs))))

  res <-
    loc %>%
    inner_join(words, by = "id") %>%
    tidyr::separate(loc, sep = ":", into = c("file", "row")) %>%
    mutate(is_rd = grepl("\\.Rd$", file)) %>%
    filter(!is_rd) %>%
    arrange(file)

  if (!missing(words_to_exclude))
    res <- res %>% filter(!(word %in% words_to_exclude))

  if (!missing(words_to_include))
    res <- res %>% filter(word %in% words_to_include)

  res %>% select(-is_rd, id)
}

fs <- fix_spelling()

View(fs)

wti <- c(
  "alf", "ile", "freiburg", "cells’", "uthority", "csv",
  "bathydemersal", "bathypelagic", "aquamapsdata’"
)

fix_spelling(words_to_include = wti)

urlchecker::url_check()
