#!/bin/bash

emacs --version
cask version
cask install --dev
cask build
cask clean-elc
cask exec ert-runner --debug
bash <(curl -s https://codecov.io/bash) -f coverage-final.json
