#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    open _coverage/index.html
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
        # WSL
        DOCPATH=$(wslpath -w ./_coverage/index.html)
        explorer.exe ${DOCPATH} || true
        # Why `|| true`? For unknown reasons, explorer.exe returns error code 1 even
        # when it succeeds in opening the path in a window.
    else
        nautilus _coverage/index.html
    fi
fi
