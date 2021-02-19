FROM rocker/geospatial:4.0.3

COPY ./rocker_scripts /rocker_scripts/

RUN chmod +x /rocker_scripts/* && \
    ./rocker_scripts/install_ccache.sh && \
    ./rocker_scripts/install_pak.sh

#RUN R -e 'pak::pkg_install("raquamaps/aquamapsdata", ask = FALSE)'

RUN installGithub.r --deps \
  raquamaps/aquamapsdata
