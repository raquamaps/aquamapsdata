#!/usr/bin/Rscript

# perform full download, processing and generation of SQLite db

library(aquamapsdata)

if (!http_ping("http://archive.org"))
  stop("Error, no online connection to Internet!")

#aquamapsdata_path <- aquamapsdata:::am_db_path()
#remote_update(AM_DB = aquamapsdata_path, truncate = FALSE)

download_db()
