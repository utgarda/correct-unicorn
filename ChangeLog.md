# Changelog for correct-unicorn
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0] - 2025-01-23
### Added
- Pass password manager integration (`--pass PATH`, `-P PATH`)
- Force overwrite flag (`--pass-force`, `-F`) for existing pass entries
- Quiet mode (`--quiet`, `-q`) suppresses all output
- Max word length filter (default: 12 chars, configurable via config.toml)
- Detailed security statistics with filtering breakdown
- Comprehensive pass integration documentation (docs/pass-integration.md)
- Security guide explaining GPG encryption model and trust
- Word filtering by apostrophes (filters contractions/possessives)

### Changed
- Min word length default changed from 2 to 3 chars
- Security stats now show detailed filtering breakdown (apostrophes, too short, too long)
- Clipboard feature marked as out of scope (deferred to password managers)

### Fixed
- Pass entry existence check (prevents accidental overwrites without --pass-force)
- Quiet mode now suppresses both stdout and stderr messages
- Removed unimplemented clipboard flag from CLI

### Security
- Pass integration can only INSERT passwords (uses GPG public key)
- Cannot read existing passwords (requires GPG private key passphrase)
- Passphrases never appear in process arguments or shell history
- ANSI codes stripped before insertion into pass

## [0.1.3] - 2025-01-14
### Added
- Security statistics flag `-z` with entropy and crack time estimates
- Property-based tests with Hedgehog (11 new tests)

### Fixed
- Reordered dictionary paths for macOS compatibility
- Applied idiomatic Haskell patterns and fixed linting issues

## [0.1.2] - 2024-11-13
### Added
- Interactive mode `-I` to show dictionary status
- Capitalize flag `--capitalize`

### Fixed
- Dictionary path validation
- System config installation on macOS

## [0.1.1] - 2024-11-12
### Fixed
- Stack GHC auto-install for macOS

## [0.1.0] - 2024-11-12
### Added
- Initial release with passphrase generation
- Dictionary auto-discovery
- ANSI colors and TOML configuration
