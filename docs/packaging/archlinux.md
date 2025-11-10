# Arch Linux Packaging

See [common packaging guide](../packaging.md) for general information.

## AUR Package

PKGBUILD is maintained in the repository root.

Building and installing:
```bash
makepkg -si
```

## Dependencies

- **Runtime**: `words` (dictionary at `/usr/share/dict/english`, 2,163 words from correctpony)
- **Build**: `stack git`

## PKGBUILD Maintenance

Update after new release:
1. Update `pkgver` to match git tag (without 'v' prefix)
2. Update checksums: `updpkgsums`
3. Test build: `makepkg -si`
