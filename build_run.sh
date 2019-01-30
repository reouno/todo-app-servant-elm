#!/bin/sh
set -eu

cd frontend
yarn webpack
cd ../
stack build
stack exec todo-app-servant-elm-exe
