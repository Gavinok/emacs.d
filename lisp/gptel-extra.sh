#!/bin/sh
emacs -batch -e "package-initialize" -l ./gptel-extra.el -l gptel-extra-test.el -f ert-run-tests-batch-and-exit
