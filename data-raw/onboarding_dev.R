library(usethis)
library(devtools)

# for adding codemeta.json
install.packages("codemetar", dependencies = TRUE)

# for adding repostatus badge
remotes::install_github("ropensci/rodev", dependencies = TRUE)

codemetar::give_opinions()
codemetar::write_codemeta()

use_lifecycle_badge("experimental")

# TODO: repo topics on GitHub?

Sys.setenv(NOT_CRAN = "true")
#file.edit("~/.Renviron")

browseURL("https://devguide.ropensci.org")

#install.packages("goodpractice")
goodpractice::gp()

# add covr
#install.packages("covr")
library(covr)
report()

# add pkgdown
#install.packages("pkgdown")


# add code of conduct to README.Rmd

# TODO: add pkgdown build from github actions
# publish /docs
