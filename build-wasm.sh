#!/usr/bin/env bash
# build-wasm.sh — compile cert-exe to WebAssembly and assemble the static site.
#
# Prerequisites
# -------------
# 1. Install the GHC WASM toolchain (one-time, ~5 min):
#
#      curl https://gitlab.haskell.org/haskell/ghc-wasm-meta/-/raw/master/bootstrap.sh \
#        | GHCUP_SKIP_UPDATE_CHECK=1 sh
#      source ~/.ghc-wasm/env          # add wasm32-wasi-ghc etc. to PATH
#
#    Add the `source` line to ~/.bashrc so it persists across shells.
#
# 2. Make this script executable:
#      chmod +x build-wasm.sh
#
# Usage
# -----
#   ./build-wasm.sh               # build + assemble ./dist/
#   ./build-wasm.sh --serve       # build + serve locally on http://localhost:8080

set -euo pipefail

DIST="dist"
PROJECT_FILE="cabal.project.wasm"

echo "==> [1/4] Building with wasm32-wasi-cabal …"
wasm32-wasi-cabal \
  --project-file="$PROJECT_FILE" \
  build exe:cert-exe

# Locate the compiled WASM binary produced by cabal.
WASM_BIN=$(wasm32-wasi-cabal \
  --project-file="$PROJECT_FILE" \
  list-bin exe:cert-exe 2>/dev/null)

echo "    Binary: $WASM_BIN"

echo "==> [2/4] Running GHC WASM post-linker (generates ghc_wasm_jsffi.js) …"
GHC_LIBDIR=$(wasm32-wasi-ghc --print-libdir)
node "$GHC_LIBDIR/post-link.mjs" \
  --input  "$WASM_BIN" \
  --output /tmp/ghc_wasm_jsffi.js

echo "==> [3/4] Assembling static site in ./$DIST/ …"
mkdir -p "$DIST"
cp "$WASM_BIN"           "$DIST/cert-exe.wasm"
cp /tmp/ghc_wasm_jsffi.js "$DIST/ghc_wasm_jsffi.js"
cp static/index.html      "$DIST/index.html"

echo "==> [4/4] Done.  Static site is in ./$DIST/"
echo ""
echo "    Files:"
ls -lh "$DIST"
echo ""

# Optional: serve locally.
if [[ "${1:-}" == "--serve" ]]; then
  echo "==> Serving at http://localhost:8080 (Ctrl-C to stop) …"
  # Use Python's built-in server; any static file server works.
  python3 -m http.server 8080 --directory "$DIST"
fi

