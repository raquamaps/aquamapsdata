[![Build
Status](https://travis-ci.org/raquamaps/aquamapsdata.svg?branch=master)](https://travis-ci.org/raquamaps/aquamapsdata)  
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- For later: [![](https://badges.ropensci.org/<issue_id>_status.svg)](https://github.com/ropensci/software-review/issues/<issue_id>) -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
``` console
Welcome to ...
  _.  _.      _. ._ _   _. ._   _  _|  _. _|_  _.
 (_| (_| |_| (_| | | | (_| |_) _> (_| (_|  |_ (_|
       |                   |
```

Introduction
------------

`aquamapsdata` is an R package that can download and create a local
SQLite database with datasets from AquaMaps.org (2017)

These datasets are available to web browsers through
<a href="https://aquamaps.org" class="uri">https://aquamaps.org</a>, but
the `aquamapsdata` package offers an a couple of convenient functions
for accessing this data programmatically in an IDE like `RStudio`.

Installing from github
----------------------

If you want to install the latest version of the `aquamapsdata` package
from github, you can do it like so:

``` r
# First make sure you have the devtools package
# which simplifies installations from github
# Note: Windows users have to first install Rtools to use devtools

install.packages("devtools") 
library(devtools)
install_github("raquamaps/aquamapsdata")
```

Quick start
-----------

Load the package in your R environment:

``` r

library(aquamapsdata)
library(purrr)

download_db(force = TRUE)

my_db <- aquamapsdata:::src_sqlite_aquamapsdata()

my_db %>% tbl("nativemaps")
my_db %>% tbl("hcaf")
my_db %>% tbl("hspen")
my_db %>% tbl("occ")
my_db %>% tbl("taxa")
```

To see some quick usage examples to get you started, open the Vignette.

The package uses SQLite3 - a portable and fast database which is
included in the RSQLite package.

You can also install SQLite3 using your platform’s package manager, for
example using these commands:

``` console
# on linux
sudo apt install sqlite3

# on mac
brew install sqlite3
```

Similar packages
----------------

A similar package which also provides the aquamaps algorithm is
available at
<a href="https://github.com/raquamaps/raquamaps" class="uri">https://github.com/raquamaps/raquamaps</a>.

References
----------

`aquamapsdata` provides data output based on tools and work described
for example in the following publications:

-   Kaschner, K., R. Watson, A.W. Trites and D. Pauly. 2006. Mapping
    worldwide distributions of marine mammals using a Relative
    Environmental Suitability (RES) model. Mar. Ecol. Prog. Ser.
    316:285-310.

-   Ready, J., K. Kaschner, A.B. South, P.D. Eastwood, T. Rees, J.
    Rius, E. Agbayani, S. Kullander, and R. Froese. 2010. Predicting the
    distributions of marine organisms at the global scale. Ecol. Model.
    221: 467-478,
    <a href="doi:10.1016/j.ecolmodel.2009.10.025" class="uri">doi:10.1016/j.ecolmodel.2009.10.025</a>

-   Kesner-Reyes, K., K. Kaschner, S. Kullander, C. Garilao, J. Barile,
    and R. Froese. 2012. AquaMaps: algorithm and data sources for
    aquatic organisms. In: Froese, R. and D. Pauly. Editors. 2012.
    FishBase. World Wide Web electronic publication. www.fishbase.org,
    version (04/2012).

-   Östergren J, Kullander S O, Prud’homme O, Reyes K K, Kaschner K and
    Froese R (in preparation) Predicting freshwater-dependent species
    distributions in Europe

Credits
-------

`aquamapsdata` would have not been possible without many amazing R
libraries, such as

-   “tidyverse” ie dplyr, tidyr, stringr etc from Hadley Wickham
-   rgbif etc from ROpenSci

Meta
----

-   Please [report any issues or
    bugs](https://github.com/raquamaps/aquamapsdata/issues).
-   License: AGPL
