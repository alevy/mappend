#!/bin/bash

if [[ $# -lt 1 ]]
then
  echo "$0 [PATH_TO_SIMPLE_REPO]"
  exit 1
fi

cabal sandbox init
cabal sandbox add-source $1/simple
cabal sandbox add-source $1/simple-templates
cabal sandbox add-source $1/simple-session
cabal sandbox add-source $1/simple-postgresql-orm
