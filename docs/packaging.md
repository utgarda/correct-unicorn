# Packaging Guide

This document contains common packaging information. See system-specific guides:
- [Arch Linux](packaging/archlinux.md) - PKGBUILD in repository root
- [Debian/Ubuntu](packaging/debian.md)
- [macOS](packaging/macos.md)
- [Dependencies](packaging/dependencies.md)

## Configuration Files

### User Configuration
`~/.config/correct-unicorn/config.toml` - UI preferences (colors, separator, bold)

### System Configuration
`/etc/correct-unicorn/config.toml` - Security settings (dict paths, min words, substitutions)

See `docs/example-config.toml` for full examples.

## Release Process

1. Update version in `package.yaml`
2. Update `ChangeLog.md`
3. Commit: `git commit -m "chore: bump version to X.Y.Z"`
4. Create annotated tag: `git tag -a vX.Y.Z -m "Release vX.Y.Z: description"`
5. Push: `git push && git push origin vX.Y.Z`
6. Update packaging files with new version/checksums

## Building from Source (All Platforms)

```bash
git clone https://github.com/utgarda/correct-unicorn.git
cd correct-unicorn
stack build
```

Binary location: `$(stack path --local-install-root)/bin/correct-unicorn`

## Installation Paths

| File | Path | Purpose |
|------|------|---------|
| Binary | `/usr/bin/correct-unicorn` or `/usr/local/bin/` | Executable |
| System config | `/etc/correct-unicorn/config.toml` | System-wide settings |
| User config | `~/.config/correct-unicorn/config.toml` | User preferences |
| Documentation | `/usr/share/doc/correct-unicorn/` | Docs |
| License | `/usr/share/licenses/correct-unicorn/LICENSE` | License file |
| Example config | `/usr/share/correct-unicorn/example-config.toml` | Config template |

## Testing Installation

```bash
# Basic test
correct-unicorn -w 4 -p

# Help text
correct-unicorn --help

# All features
correct-unicorn -w 3 -s "-" -c 20 --capitalize -p
```
