#!/usr/bin/Rscript

# perform full download, processing and generation of SQLite db

library(aquamapsdata)

if (!http_ping())
  stop("Error, no online connection to aquamaps.org!")

aquamapsdata_path <- aquamapsdata:::am_db_path()

remote_update(AM_DB = aquamapsdata_path, truncate = FALSE)
