#!/usr/bin/env bash

rm -rf venv/
virtualenv venv
./venv/bin/pip install -r requirements.txt
