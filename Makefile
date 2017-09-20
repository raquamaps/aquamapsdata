#! make

PKG_DIR=$(shell Rscript -e "cat(.libPaths()[1])")/aquamapsdata/extdata
DB_CACHE=~/am.db

all:
	@echo "aquamaps R package extdata dir is:"
	@echo $(PKG_DIR)
	@echo "contents:"
	@ls -1 $(PKG_DIR)

backup:
	cp $(PKG_DIR)/am.db .

restore:
	mkdir -p $(PKG_DIR)
	ln -s $(DB_CACHE) $(PKG_DIR)
	# to support Vignette build during package development
	mkdir -p inst/extdata
	ln -s $(DB_CACHE) inst/extdata/am.db

download-db:
	Rscript exec/cli_update.R

am.db.gz:
	gzip am.db

upload-ia: am.db.gz
	ia upload aquamapsdata am.db.gz \
		--metadata="title:AquaMaps.org Datasets"

download-ia:
	ia download aquamapsdata am.db.gz \
		--destdir=. --no-directories
	gunzip am.db.gz

clean:
	rm -f am.db am.db.gz $(PKG_DIR)/am.db inst/extdata/am.db
	rmdir inst/extdata
