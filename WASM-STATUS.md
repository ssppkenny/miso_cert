# WASM Build Status

## Current Situation

The WASM build compiles successfully but the Miso app doesn't render to the DOM.

### Root Cause

There's a type mismatch between what Miso's `startApp` returns and what we need for WASM:
- `startApp :: App model action -> JSM ()` (returns JSM monad)
- WASM `main` needs: `main :: IO ()`

### What Works

✅ WASM binary compiles (3.9 MB)  
✅ FFI bridge loads  
✅ WASM module instantiates  
✅ `hs_start` function exports successfully  
✅ No runtime errors when calling `hs_start`  

### What Doesn't Work  

❌ Miso app doesn't render to DOM  
❌ No way to convert `JSM ()` to `IO ()` that actually executes the action  
❌ `unsafeCoerce` doesn't work (doesn't execute, just changes types)  
❌ `syncPoint` has wrong type signature  

## Attempted Solutions

1. **unsafeCoerce** - Doesn't execute the JSM action, just type-casts
2. **syncPoint** - Wrong type signature (`JSM () -> JSM ()` not `JSM () -> IO ()`)
3. **Removing `-jsaddle` flag** - Miso still uses JSM types
4. **Different entry points** - `_start`, `hs_main`, `main` - all have same issue

## Possible Solutions

### Option 1: Use Older Miso Version
The miso-sampler example uses a different API where `startApp` might return `IO ()` directly.

### Option 2: Use GHCJS Instead of GHC WASM
GHCJS is more mature for Miso apps and has better support.

### Option 3: Investigate Miso's WASM Backend
Study how the miso-sampler actually works and replicate its exact setup.

### Option 4: Native Build Only
Use the jsaddle-warp version which works perfectly:
```bash
stack build
stack exec cert-exe  # serves on http://localhost:3000
```

## Recommendation

For now, use the **native build with jsaddle-warp** which works flawlessly. The WASM build path needs more investigation into Miso's architecture or potentially using a different framework/version.

The native build is:
- ✅ Fully functional
- ✅ Easy to develop with (fast rebuilds)
- ✅ Can be deployed with the warp server

## Files Modified

- `app/Main.hs` - Added WASM conditional compilation
- `package.yaml` - Made ghc-options conditional  
- `cabal.project.wasm` - WASM build configuration
- `static/index.html` - WASM loader with WASI polyfills
- `build-wasm.sh` - Build script for WASM

All changes are backward compatible with the native build.

