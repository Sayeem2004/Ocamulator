#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]; then
    open src/_coverage/index.html
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
        DOCPATH=$(wslpath -w ./src/_coverage/index.html)
        explorer.exe ${DOCPATH} || true
    else
        nautilus src/_coverage/index.html
    fi
fi
