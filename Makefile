#! make

PKG_DIR=$(shell Rscript -e "cat(.libPaths()[1])")/aquamapsdata/extdata

all:
	@echo $(PKG_DIR)

backup:
	cp $(PKG_DIR)/am.db /tmp/am.db

restore:
	mkdir -p $(PKG_DIR)
	cp /tmp/am.db $(PKG_DIR)/am.db

download:
	Rscript exec/cli_update.R

/tmp/am.db.gz:
	gzip /tmp/am.db

upload-ia: /tmp/am.db.gz
	ia upload aquamapsdata /tmp/am.db.gz \
		--metadata="title:AquaMaps.org Datasets"

download-ia:
	ia download aquamapsdata am.db.gz \
		--destdir=/tmp --no-directories
	gunzip /tmp/am.db.gz

clean:
	rm -f inst/extdata/*
