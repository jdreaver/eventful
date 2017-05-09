#!/usr/bin/env bash

set -eu;

if [ "$#" -ne 1 ]; then
  echo "Usage: ./scripts/bump-version <version>"
  exit 1
fi

version="$1"

for file in eventful-*/package.yaml; do
  sed -i "s/version:.*/version: \"$version\"/" "$file"
done
