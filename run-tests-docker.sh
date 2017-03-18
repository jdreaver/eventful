#!/usr/bin/env bash

set -eux;

# Spin up a docker container to run postgres tests
if [ ! "$(docker ps -q -f name=eventful_test_pg)" ]; then
  if [ "$(docker ps -aq -f name=eventful_test_pg)" ]; then
    docker rm eventful_test_pg
  fi
  docker run --name eventful_test_pg -e POSTGRES_DB=eventful_test -p 5432:5432/tcp -d postgres:9.6 postgres

  # Give time for postgres to start
  sleep 10
fi

# Spin up a docker container for dynamodb
if [ ! "$(docker ps -q -f name=eventful_test_dynamo)" ]; then
  if [ "$(docker ps -aq -f name=eventful_test_dynamo)" ]; then
    docker rm eventful_test_dynamo
  fi
  docker run -d --name eventful_test_dynamo -p 8000:8000 dwmkerr/dynamodb
fi

# Set dummy env vars for dynamo
export AWS_ACCESS_KEY_ID=dummy
export AWS_SECRET_ACCESS_KEY=dummy
export AWS_DEFAULT_REGION=us-east-1

stack test --pedantic "$@"
