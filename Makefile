#! make

PKG_DIR=$(shell Rscript -e "cat(.libPaths()[1])")/aquamapsdata/extdata

all:
	@echo "aquamaps R package extdata dir is:"
	@echo $(PKG_DIR)
	@echo "contents:"
	@ls -1 $(PKG_DIR)

backup:
	cp $(PKG_DIR)/am.db .

restore:
	mkdir -p $(PKG_DIR)
	cp am.db $(PKG_DIR)

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
	rm -f am.db am.db.gz $(PKG_DIR)/am.db
