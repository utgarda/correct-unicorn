# Debian/Ubuntu Packaging

See [common packaging guide](../packaging.md) for general information.

## Building from Source

```bash
# Install dependencies
sudo apt-get install wamerican haskell-stack git build-essential

# Clone and build
git clone https://github.com/utgarda/correct-unicorn.git
cd correct-unicorn
stack build

# Install
sudo install -Dm755 $(stack path --local-install-root)/bin/correct-unicorn /usr/bin/correct-unicorn
sudo install -Dm644 etc/correct-unicorn/config.toml /etc/correct-unicorn/config.toml
sudo install -Dm644 docs/*.md /usr/share/doc/correct-unicorn/
sudo install -Dm644 LICENSE /usr/share/licenses/correct-unicorn/LICENSE
```

## Dependencies

- **Runtime**: `wamerican` (dictionary at `/usr/share/dict/american-english`)
- **Build**: `haskell-stack git build-essential`

## Future: .deb Package

Detailed .deb packaging instructions to be added.
