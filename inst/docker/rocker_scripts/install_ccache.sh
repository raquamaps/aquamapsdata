#!/bin/bash

set -e

# install and configure ccache

mkdir $HOME/.R

printf 'CXX_STD = CXX14\n\nVER=\nCCACHE=ccache\nCC=$(CCACHE) gcc$(VER) -std=gnu99\nCXX=$(CCACHE) g++$(VER)\nC11=$(CCACHE) g++$(VER)\nC14=$(CCACHE) g++$(VER)\nFC=$(CCACHE) gfortran$(VER)\nF77=$(CCACHE) gfortran$(VER)' > $HOME/.R/Makevars

mkdir -p $HOME/.ccache

printf 'max_size = 5.0G\nsloppiness = include_file_ctime\nhash_dir=false' > $HOME/.ccache/ccache.conf

apt-get update -y && apt-get install -y --no-install-recommends \
    ccache
