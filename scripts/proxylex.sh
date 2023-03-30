#!/bin/sh
sbcl --load "$(dirname "$(realpath "$0")")/../src/proxylex-main.lisp"