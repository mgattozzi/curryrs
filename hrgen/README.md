Easily generate Haskell or Rust function bindings for exported functions
for FFI.

This tool is in alpha right now and hasn't been released for general
consumption. Use at your own risk.

To use the tool:

```bash
git clone github.com/mgattozzi/curryrs
cd curryrs/hrgen
cargo install

# Or your haskell file with foreign exports
hrgen -h Test.hs
```

Things that don't work now:
- Whole directories
- Rust to Haskell
