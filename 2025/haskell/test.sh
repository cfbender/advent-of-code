#! /bin/bash

stack test --ta "--match \"Day $1\"" --file-watch
