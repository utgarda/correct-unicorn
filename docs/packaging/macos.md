# macOS Packaging

See [common packaging guide](../packaging.md) for general information.

## Homebrew Installation

From this repository:
```bash
brew install Formula/correct-unicorn.rb
```

Future (via tap):
```bash
brew tap utgarda/correct-unicorn
brew install correct-unicorn
```

## Dependencies

- **Runtime**: Built-in dictionary at `/usr/share/dict/words` (235k words)
- **Build**: `brew install haskell-stack git`

## Building from Source

```bash
git clone https://github.com/utgarda/correct-unicorn.git
cd correct-unicorn
stack build
cp $(stack path --local-install-root)/bin/correct-unicorn /usr/local/bin/
```

## Formula Maintenance

Update sha256 after new release:
```bash
curl -sL https://github.com/utgarda/correct-unicorn/archive/refs/tags/vX.Y.Z.tar.gz | shasum -a 256
```

Test formula:
```bash
brew install --build-from-source Formula/correct-unicorn.rb
brew test correct-unicorn
```
