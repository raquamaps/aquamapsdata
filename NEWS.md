NEWS
====

For more fine-grained list of changes or to report a bug, consult 

* [The issues log](https://github.com/raquamaps/aquamapsdata/issues)
* [The commit log](https://github.com/raquamaps/aquamapsdata/commits/master)


# v0.1.2

* Added more content to the Vignette to illustrate the database schema and show how to make some queries against the database

* Added a .travis.yml file to the project with the intention to support that Travis CI can build and deploy to GitHub Releases when a tag is pushed using git

# v0.1.1

* Moved csv-to-sqlite conversion and put it in data-raw to avoid loading all those dependencies in the package

* Introduced download_db() function which downloads a pre-processed SQLite3 db file from the Internet Archive

* Made sure there is a "am.db.gz" available from the Internet Archive at archive.org/downloads/aquamapsdata/amd.db.gz





