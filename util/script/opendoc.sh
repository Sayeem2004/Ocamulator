#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]; then
    open src/_build/default/_doc/_html/Ocamulator/index.html
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
        DOCPATH=$(wslpath -w ./src/_build/default/_doc/_html/Ocamulator/index.html)
        explorer.exe ${DOCPATH} || true
    else
        nautilus src/_build/default/_doc/_html/Ocamulator/index.html
    fi
fi
