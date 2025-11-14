# Changelog for correct-unicorn
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
