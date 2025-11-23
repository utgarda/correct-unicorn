# correct-unicorn

Passphrase generator inspired by [xkcd 936](https://xkcd.com/936/) and [correctpony](https://github.com/maandree/correctpony).

Generates memorable passphrases from system dictionaries with ANSI color output.

## Prerequisites

### Arch Linux

```bash
sudo pacman -S words stack
```

### Ubuntu 22.04 / 24.04 LTS

```bash
sudo apt-get install wamerican haskell-stack
```

### macOS

```bash
# Dictionary pre-installed
brew install haskell-stack
```

## Quick Start

```bash
# Build
stack build

# Generate passphrase
stack exec -- correct-unicorn --words 4
# Output: correct horse battery staple

# Interactive mode (view available dictionaries)
stack exec -- correct-unicorn --interactive
```

## Pass Integration

Generate passphrases and insert directly into [pass](https://www.passwordstore.org/):

```bash
# Generate and store in pass
correct-unicorn --words 5 --pass github.com/username

# With custom options
correct-unicorn -w 6 -s "-" --capitalize --pass work/email
```

See [docs/pass-integration.md](docs/pass-integration.md) for setup guide.

## Features

- Auto-discovers system dictionaries
- Validates dictionaries (uniqueness, minimum size)
- ANSI color cycling for better readability
- Interactive mode for transparency
- Minimal character substitution for complexity requirements
- Pass password manager integration

## Configuration

See [docs/configuration.md](docs/configuration.md)

- System config: `/etc/correct-unicorn/config.toml` (dictionary discovery order)
- User config: `~/.config/correct-unicorn/config.toml` (UI preferences only)

## Documentation

- [Installation Guide](docs/installation.md)
- [Pass Integration](docs/pass-integration.md)
- [Dictionary Management](docs/dictionaries.md)
- [Configuration](docs/configuration.md)
- [Features Roadmap](docs/features.md)
- [Packaging](docs/packaging/)

## Release Verification

Releases are signed with GPG key:
```
Key ID: 3B00CAB0BD4D8648
Fingerprint: 37532468A03463C48B7631403B00CAB0BD4D8648
```

Verify downloads:
```bash
# Import public key
gpg --keyserver keys.openpgp.org --recv-keys 3B00CAB0BD4D8648

# Verify signature
gpg --verify correct-unicorn-VERSION.tar.gz.asc correct-unicorn-VERSION.tar.gz

# Verify checksums
sha256sum -c SHA256SUMS
```

Releases also include [SLSA provenance](https://slsa.dev/) for supply chain verification.

## License

MIT
