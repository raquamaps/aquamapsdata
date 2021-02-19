#! make

all: build

build:
	# docker buildx command (docker ce 19.03 and onwards)
	docker build --load \
		--cache-from type=registry,ref=rocker/geospatial:4.0.3 \
		--cache-to type=inline \
		-t raquamaps/aquamapsdata:rocker .

push:
	docker push raquamaps/aquamapsdata:rocker

browse:
	echo "Is the full aquamaps db is available locally?"
	echo "Please open http://localhost:8787 in your browser"
	docker run --rm \
		-p 8787:8787 \
		-e PASSWORD=aquamapsdata \
		-v $$HOME/.config/aquamaps/am.db:/home/rstudio/.config/aquamaps/am.db:ro \
		raquamaps/aquamapsdata:rocker

up:
	docker-compose up -d
	sleep 5 && xdg-open https://aquamapsdb.org/rstudio &

down:
	docker-compose down
