# aquamapsdata

For more fine-grained list of changes or to report a bug, consult 

* [The issues log](https://github.com/raquamaps/aquamapsdata/issues)
* [The commit log](https://github.com/raquamaps/aquamapsdata/commits/master)

## v0.1.6

Changes are listed [here](https://github.com/raquamaps/aquamapsdata/issues?q=is%3Aissue+milestone%3A%22RC+1%22+is%3Aclosed) and include for example:

* Refreshed data

* Updated CI/CD GitHub Actions 

* Various changes for better compliance with ROpenSci package development guidelines

## v0.1.5
 
* Added GitHub Actions for R-CMD-check and test-coverage.

* Added pkgdown site.

* Updated docs with info about citation, content licensing (CC-BY-SA v3) and code of conduct.

* Fixed a bug "row value misused" from RSQLite probably due to lazy eval of query in db, for a query that was generated looking like this: "SELECT * FROM `taxa` WHERE (`Genus` = CASE WHEN (1.0) THEN (('Salmo', 'trutta')) END AND `Species` = CASE WHEN (2.0) THEN (('Salmo', 'trutta')) END)"

## v0.1.4

* Fixed a bug in install_db(), which didn't overwrite an existing db

* Added csquarecode <-> decimal degrees encoding and decoding functions

# Removed use of stringr and use stringi instead

# v0.1.3

* Added functions for fuzzy and exact name searches based on FTS capabilities of SQLite3, some tests for those and usage example in the Vignette

* Removed functions hspen(), occ(), taxa(), hcaf() - those need refactoring and will be added when the column names and descriptions have been revised

* Added in a smaller "am.db" in inst/extdata which gets built into and installed within the package. This simplifies the building and checking of the package, as well as running tests. The intention is to have download_db() replace that database.

# v0.1.2

* Added more content to the Vignette to illustrate the database schema and show how to make some queries against the database

* Added a .travis.yml file to the project with the intention to support that Travis CI can build and deploy to GitHub Releases when a tag is pushed using git

# v0.1.1

* Moved CSV-to-SQLite conversion and put it in data-raw to avoid loading all those dependencies in the package

* Introduced download_db() function which downloads a pre-processed SQLite3 db file from the Internet Archive

* Made sure there is a "am.db.gz" available from the Internet Archive at archive.org/downloads/aquamapsdata/amd.db.gz
