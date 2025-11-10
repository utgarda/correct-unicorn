# Installation Guide

## Prerequisites

### Arch Linux

```bash
# Install dictionary and build tools
sudo pacman -S words stack git

# words package provides /usr/share/dict/english
```

### Ubuntu 22.04 / 24.04 LTS

```bash
# Install dictionary and build tools
sudo apt-get install wamerican haskell-stack git

# wamerican provides /usr/share/dict/american-english
# Note: NOT installed by default
```

### macOS

```bash
# Dictionary pre-installed at /usr/share/dict/words

# Install build tools
brew install haskell-stack git
```

## Building from Source

```bash
# Clone repository
git clone https://github.com/utgarda/correct-unicorn.git
cd correct-unicorn

# Build with Stack
stack build

# Run
stack exec -- correct-unicorn --words 4
```

## Installing from Package

### Arch Linux (AUR)

```bash
# Install from AUR
yay -S correct-unicorn
# or
paru -S correct-unicorn

# Run
correct-unicorn --words 4
```

### macOS (Homebrew)

```bash
# Install from local formula
brew install Formula/correct-unicorn.rb

# Or from tap (future)
brew tap utgarda/correct-unicorn
brew install correct-unicorn

# Run
correct-unicorn --words 4
```

See [packaging/macos.md](packaging/macos.md) for details.

### Ubuntu/Debian

```bash
# Build .deb from source (instructions in docs/packaging/debian.md)
# or download pre-built .deb from releases

sudo dpkg -i correct-unicorn_0.1.0-1_amd64.deb
sudo apt-get install -f  # Install dependencies

# Run
correct-unicorn --words 4
```

## Verification

Check if dictionary is installed:

```bash
# Should show a word list file
ls -l /usr/share/dict/words

# Count words in dictionary
wc -l /usr/share/dict/words
```

Test correct-unicorn:

```bash
correct-unicorn --interactive
# Should show available dictionaries
```

## Troubleshooting

**Dictionary file not found:**

1. Install dictionary package (see Prerequisites above)
2. Or specify custom wordlist: `correct-unicorn --dict /path/to/wordlist.txt`
3. Check available dictionaries: `correct-unicorn --interactive`

**Build fails:**

- Ensure Stack is updated: `stack upgrade`
- Clean and rebuild: `stack clean && stack build`

## See Also

- [dictionaries.md](dictionaries.md) - Dictionary management and validation
- [configuration.md](configuration.md) - Configuration options
- [packaging.md](packaging.md) - Common packaging guide
- [packaging/](packaging/) - Platform-specific packaging instructions
