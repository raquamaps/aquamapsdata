<!-- badges: start -->

[![Build
Status](https://travis-ci.org/raquamaps/aquamapsdata.svg?branch=master)](https://travis-ci.org/raquamaps/aquamapsdata)
![R-CMD-check](https://github.com/raquamaps/aquamapsdata/workflows/R-CMD-check/badge.svg)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/raquamaps/aquamapsdata/branch/master/graph/badge.svg)](https://codecov.io/gh/raquamaps/aquamapsdata?branch=master)
[![peer-review](https://badges.ropensci.org/421_status.svg)](https://github.com/ropensci/software-review/issues/421)
<!-- badges: end -->

<!-- For later: [![](https://badges.ropensci.org/<issue_id>_status.svg)](https://github.com/ropensci/software-review/issues/<issue_id>) -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` console
Welcome to ...
  _.  _.      _. ._ _   _. ._   _  _|  _. _|_  _.
 (_| (_| |_| (_| | | | (_| |_) _> (_| (_|  |_ (_|
       |                   |
```

<img src="man/figures/sticker.png" align="right" />

Introduction
------------

`aquamapsdata` is an R package that can download and create a local
SQLite database with datasets from AquaMaps.org.

These datasets are available to web browsers through
<a href="https://aquamaps.org" class="uri">https://aquamaps.org</a>, but
the `aquamapsdata` package offers an a couple of convenient functions
for accessing this data programmatically in an IDE like `RStudio`.

Installing from github
----------------------

If you want to install the latest version of the `aquamapsdata` package
from github, you can do it like so:

``` r
remotes::install_github("raquamaps/aquamapsdata", dependencies = TRUE)
```

Quick start
-----------

Load the package in your R environment:

``` r
library(aquamapsdata)

# downloads about 2 GB of data, approx 10 GB when unpacked
download_db()
default_db("sqlite")

am_citation()
```

To see some quick usage examples to get you started, open the Vignette.

The package uses SQLite3 - a portable and fast database which is
included in the RSQLite package.

Other useful packages
---------------------

-   To further work with the data, packages such as `sp`, `raster` and
    other geospatial R packages can be useful.

-   For looking up other data related to the included species, packages
    such as `taxizedb`, `rgbif`, `rfishbase` can be useful.

Data source, Citation and References
------------------------------------

`aquamapsdata` provides data output from
<a href="https://aquamaps.org" class="uri">https://aquamaps.org</a>;
when using data provided by the package in a publication, please cite
this source:

-   Kaschner, K., K. Kesner-Reyes, C. Garilao, J. Segschneider, J.
    Rius-Barile, T. Rees, and R. Froese. 2019. AquaMaps: Predicted range
    maps for aquatic species. World wide web electronic publication,
    www.aquamaps.org, Version 10/2019.

### Content - Copyright and Disclaimer

Content from AquaMaps as provided by functions in this R package is
licensed under a [Creative Commons Attribution-NonCommercial 3.0
Unported License](http://creativecommons.org/licenses/by-nc/3.0/):
<img src="https://i.creativecommons.org/l/by-nc/3.0/80x15.png" style="border-width:0" />

You are welcome to include text, numbers and maps from AquaMaps in your
own work for non-commercial use, given that such inserts are clearly
identified as coming from AquaMaps.org, with a backward link to the
respective source.

### Considerations before publication

A researcher planning publication based on these datasets is invited to
contact the AquaMaps team with queries related to specific content. The
AquaMaps team can help with double checking for correctness prior to
drawing conclusions and/or subsequent publication, and provide further
clarification of update frequency, limitations and/or interpretation of
unlikely results.

Please open an issue on the [GitHub issue
tracker](https://github.com/raquamaps/aquamapsdata/issues) or contact
the team directly by email at Rainer Froese (rfroese (at) geomar.de) or
Kristin Kaschner (Kristin.Kaschner (at) biologie.uni-freiburg.de).

### Disclaimer

AquaMaps generates standardized computer-generated and fairly reliable
large scale predictions of marine and freshwater species. Although the
AquaMaps team and their collaborators have obtained data from sources
believed to be reliable and have made every reasonable effort to ensure
its accuracy, many maps have not yet been verified by experts and we
strongly suggest you verify species occurrences before usage. The
AquaMaps team will not be held responsible for any consequence from the
use or misuse of these data and/or maps by any organization or
individual.

Meta
----

-   Please [report any issues or
    bugs](https://github.com/raquamaps/aquamapsdata/issues).
-   License: AGPL
-   Get citation information for `aquamapsdata` in R by doing
    citation(package = ‘aquamapsdata’)
-   Please note that this package is released with a [Contributor Code
    of Conduct](https://ropensci.org/code-of-conduct/). By contributing
    to this project, you agree to abide by its terms.
-   Credits: `aquamapsdata` would have not been possible without many
    amazing R libraries, such as the “tidyverse” packages and various
    packages from ROpenSci.
