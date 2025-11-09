# Dependencies

## Runtime Dependencies

| Dependency | Arch Linux | Ubuntu/Debian | macOS | Purpose |
|------------|------------|---------------|-------|---------|
| Dictionary | `words` | `wamerican` | built-in | System word lists |
| GMP | `gmp` | `libgmp10` | built-in | Arbitrary precision math |

## Build Dependencies

| Dependency | Arch Linux | Ubuntu/Debian | macOS | Purpose |
|------------|------------|---------------|-------|---------|
| Stack | `stack` | `haskell-stack` | `brew install haskell-stack` | Haskell build tool |
| Git | `git` | `git` | `brew install git` | Source control |
| GMP dev | `gmp` | `libgmp-dev` | built-in | Build-time library |

## Haskell Dependencies

Managed by Stack (defined in package.yaml):

- base >= 4.7 && < 5
- optparse-applicative >= 0.14.3
- random
- tomland >= 1.3
- directory >= 1.3
- filepath >= 1.4
- text
- containers

## Future Dependencies (Planned)

| Feature | Package | Platform Notes |
|---------|---------|----------------|
| pass integration | `pass` | password-store |
| Clipboard (X11) | `xclip` | X11 systems |
| Clipboard (Wayland) | `wl-clipboard` | Wayland systems |
| Clipboard (macOS) | built-in | pbcopy/pbpaste |

## Dictionary Package Details

### Arch Linux: `words` Package
- Provides: `/usr/share/dict/english`
- Word count: ~2,163 words
- From correctpony wordlists

### Ubuntu/Debian: `wamerican` Package
- Provides: `/usr/share/dict/american-english`
- Word count: ~102,401 words (standard), more in -large/-huge variants
- Symlink at `/usr/share/dict/words`

### macOS: Built-in
- Location: `/usr/share/dict/words`
- Word count: ~235,886 words
- Based on Webster's International Dictionary (1934)
