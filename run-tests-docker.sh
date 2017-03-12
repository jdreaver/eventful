#!/usr/bin/env bash

set -eux;

# Spins up a docker container to run postgres tests

if [ ! "$(docker ps -q -f name=eventful_test_pg)" ]; then
  if [ "$(docker ps -aq -f status=exited -f name=eventful_test_pg)" ]; then
    docker rm eventful_test_pg
  fi
  docker run --name eventful_test_pg -e POSTGRES_DB=eventful_test -p 5432:5432/tcp -d postgres:9.6 postgres

  # Give time for postgres to start
  sleep 10
fi

stack test --pedantic
