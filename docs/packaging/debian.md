# Debian/Ubuntu Packaging

## Building from Source

```bash
# Install dependencies
sudo apt-get install wamerican haskell-stack git build-essential

# Clone and build
git clone https://github.com/utgarda/correct-unicorn.git
cd correct-unicorn
stack build

# Install binary
sudo install -Dm755 $(stack path --local-install-root)/bin/correct-unicorn /usr/bin/correct-unicorn

# Install system config
sudo install -Dm644 etc/correct-unicorn/config.toml /etc/correct-unicorn/config.toml

# Install documentation
sudo install -Dm644 LICENSE /usr/share/licenses/correct-unicorn/LICENSE
sudo install -Dm644 docs/*.md /usr/share/doc/correct-unicorn/
```

## Creating .deb Package

(Future: detailed .deb packaging instructions)

For now, users should build from source as shown above.

## Dependencies

- **Runtime**: wamerican (dictionary)
- **Build**: haskell-stack, git, build-essential
