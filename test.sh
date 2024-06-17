#!/bin/bash

ros run --noinform --non-interactive -e "(require :asdf)" -e "(progn (load \"puzzles.asd\") (uiop:quit (if (asdf:test-system :puzzles/test) 0 -1)))"
