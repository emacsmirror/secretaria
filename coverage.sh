#!/bin/bash

if [[ ! -z "$CODECOV_TOKEN" ]]; then
    bash <(curl -s https://codecov.io/bash) -f coverage-final.json
fi
