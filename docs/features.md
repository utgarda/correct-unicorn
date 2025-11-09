# correct-unicorn Feature List

This document provides a comprehensive overview of features for correct-unicorn, including current implementation, planned features, and inspiration from similar tools (correctpony and correcthorse).

## Table of Contents
1. [Current Implementation](#current-implementation)
2. [Tool Comparison](#tool-comparison)
3. [Feature Catalog](#feature-catalog)
4. [Implementation Roadmap](#implementation-roadmap)

---

## Current Implementation

### What correct-unicorn Has Now (v0.1.0.0)

**Core Functionality:**
- Passphrase generation from dictionary words
- Configurable word count via `-w/--words COUNT` flag (default: 4)
- Dictionary source: hardcoded to `/usr/share/dict/words`
- Uses `System.Random` for word selection

**Visual Output:**
- ANSI color cycling through words (green → yellow → green → yellow...)
- Bold formatting for output
- Colors defined: green, yellow, blue (commented), magenta (commented)

**Implementation:**
- Language: Haskell
- Build system: Stack (resolver lts-14.4)
- Dependencies: base, optparse-applicative, random
- Architecture: Two modules (Main.hs, PrettyAnsi.hs)

---

## Tool Comparison

### Feature Matrix

| Feature | correctpony v2.0 | correcthorse v1.0 | correct-unicorn v0.1.0.0 | Priority |
|---------|-----------------|-------------------|--------------------------|----------|
| **Generation** |
| Word count control | ✓ `-w/--words` | ✓ `-w/--words` | ✓ `-w/--words` | ✓ Done |
| Character count minimum | ✓ `-c/--characters` | ✓ `-c/--char` | ✗ | High |
| Generate N passphrases | ✓ `[COUNT]` | ✓ `[count]` | ✗ | Medium |
| Force word inclusion | ✓ `-i/--include` | ✓ `-i/--include` | ✗ | Medium |
| **Wordlists** |
| Multiple wordlists | ✓ 19 lists | ✓ 1 list | ✗ (hardcoded) | High |
| Select wordlist | ✓ `-l/--list` | ✓ `-l/--wordlist` | ✗ | High |
| List available wordlists | ✓ `--wordlists` | ✗ | ✗ | Medium |
| Show word count | ✓ `--wordcount` | ✗ | ✗ | Low |
| **Formatting** |
| Custom separator | ✓ `-s/--separator` | ✓ `-s/--sep` | ✗ (space) | High |
| CamelCase mode | ✓ `-u/--camelcase` | ✓ `-u/--camelcase` | ✗ | Medium |
| Word joining | ✓ `-j/--join` | ✗ | ✗ | Low |
| ANSI colors | ✓ (default) | ✗ | ✓ (default) | ✓ Done |
| No-color mode | ✓ `-p/--nocolour` | ✗ | ✗ | High |
| Bold output | ✗ | ✗ | ✓ | ✓ Done |
| **Security** |
| Security statistics | ✓ `-z/--security` | ✗ | ✗ | High |
| Entropy display | ✓ (with `-z`) | ✗ | ✗ | High |
| Custom RNG source | ✓ `-r/--random` | ✗ | ✗ | Medium |
| **Integration** |
| Clipboard support | ✗ | ✗ | ✗ | **High (Planned)** |
| Pass integration | ✗ | ✗ | ✗ | **High (Planned)** |
| **UI/UX** |
| Help text | ✓ `-h/--help` | ✓ `-h/--help` | ✓ `--help` | ✓ Done |
| Version info | ✓ `--version` | ✓ `-v/--version` | ✗ | Low |
| License/warranty | ✓ `--copying/--warranty` | ✗ | ✗ | Low |
| Retro hacker aesthetics | ✗ | ✗ | ✗ | **High (Planned)** |

### Implementation Languages
- **correctpony v2.0**: Java (AGPL-3.0)
- **correcthorse v1.0**: C (WTFPL 2.0)
- **correct-unicorn v0.1.0.0**: Haskell (MIT)

---

## Feature Catalog

### 1. Passphrase Generation

#### 1.1 Word Count Control
**Status**: ✓ Implemented
**Command**: `-w COUNT`, `--words COUNT`
**Current**: Default 4 words
**Description**: Specify minimum number of words in passphrase

#### 1.2 Character Count Minimum
**Status**: ✗ Not implemented
**Priority**: High
**Command (proposed)**: `-c COUNT`, `--chars COUNT`
**Description**: Ensure passphrase meets minimum character length requirement
**Example**: `correct-unicorn -c 20` generates passphrase with at least 20 characters

#### 1.3 Multiple Passphrase Generation
**Status**: ✗ Not implemented
**Priority**: Medium
**Command (proposed)**: `correct-unicorn [COUNT]`
**Description**: Generate N passphrases in single invocation
**Example**: `correct-unicorn 5` generates 5 passphrases
**Use case**: Generate multiple options to choose from

#### 1.4 Force Word Inclusion
**Status**: ✗ Not implemented
**Priority**: Medium
**Command (proposed)**: `-i WORD`, `--include WORD`
**Description**: Ensure specific word appears in passphrase
**Example**: `correct-unicorn -i unicorn -w 4`
**Use case**: Memorable anchor words, domain-specific terms

---

### 2. Wordlist Management

#### 2.1 Configurable Dictionary Paths
**Status**: ✗ Not implemented (hardcoded to `/usr/share/dict/words`)
**Priority**: High
**Command (proposed)**: `-l WORDLIST`, `--wordlist WORDLIST`
**Description**: Select from multiple wordlist files
**Implementation notes**:
- Support absolute paths
- Support relative paths to `/usr/share/dict/`
- Allow multiple wordlists (up to 10, like correcthorse)

**Proposed wordlist locations**:
```
/usr/share/correct-unicorn/
├── english.dict          # Default comprehensive English
├── common-english.dict   # Frequently used words
├── basic-english.dict    # Simple vocabulary
├── computers.dict        # Technical terms
├── animals.dict
├── elements.dict
├── shakespeare.dict
└── ...
```

#### 2.2 List Available Wordlists
**Status**: ✗ Not implemented
**Priority**: Medium
**Command (proposed)**: `--wordlists`
**Description**: Display all available wordlists
**Options**:
- `--wordlists`: Show list names
- `--wordlists --full`: Show full file paths

#### 2.3 Word Count Display
**Status**: ✗ Not implemented
**Priority**: Low
**Command (proposed)**: `--wordcount`
**Description**: Show number of unique words in selected wordlist(s)
**Example output**: `Total words available: 2163`

---

### 3. Output Formatting

#### 3.1 Word Separators
**Status**: ✗ Not implemented (hardcoded to space)
**Priority**: High
**Command (proposed)**: `-s SEP`, `--separator SEP`
**Description**: Customize separator between words
**Examples**:
- `correct-unicorn -s "-"` → `phrase-opposite-cloth-statement`
- `correct-unicorn -s ""` → `phraseoppositeClothstatement`
- `correct-unicorn -s "."` → `phrase.opposite.cloth.statement`

#### 3.2 CamelCase Formatting
**Status**: ✗ Not implemented
**Priority**: Medium
**Command (proposed)**: `-u`, `--camelcase`
**Description**: Capitalize first letter of each word
**Example**: `correct-unicorn -u -s ""` → `PhraseOppositeClothStatement`

#### 3.3 No-Color Mode
**Status**: ✗ Not implemented
**Priority**: High
**Command (proposed)**: `-p`, `--no-color`
**Description**: Disable ANSI color codes for piping/scripting
**Use case**: Redirecting output to files, piping to other commands
**Example**: `correct-unicorn -p > password.txt`

#### 3.4 ANSI Color Cycling
**Status**: ✓ Implemented
**Current**: Green → Yellow alternating
**Enhancement**: Enable blue/magenta colors (currently commented)
**Future**: Configurable color schemes for retro hacker aesthetic

#### 3.5 Bold Formatting
**Status**: ✓ Implemented
**Note**: Currently using `\x1b[0m` (reset code) instead of proper bold `\x1b[1m`
**Bug**: app/PrettyAnsi.hs:22 should be `\x1b[1m` not `\x1b[0m`

---

### 4. Security Features

#### 4.1 Security Statistics Display
**Status**: ✗ Not implemented
**Priority**: High
**Command (proposed)**: `-z`, `--security`
**Description**: Show entropy and security estimates
**Example output** (from correctpony):
```
You have a total of 2163 words in all your selected word lists,
The English language contains about 500000 words. That is c:a 0 %.

Trying to crack the passphrase knowing all settings and the dictionaries,
it would take about 0.69 years at most, making 1,000,000 guesses per second.

Trying to crack the passphrase knowing all settings and the dictionaries,
it would take about 693.64 years at most, making 1000 guesses per second.

Trying to crack the passphrase knowing all settings and the dictionaries,
it would take about 3468175.24 years at most, making 1 guess every fifth second.
```

**Implementation requirements**:
- Calculate total keyspace from wordlist size and word count
- Display estimated crack time at various guess rates
- Show percentage of English language covered
- Optional: entropy in bits calculation

#### 4.2 Configurable Randomness Source
**Status**: ✗ Not implemented (uses `System.Random.getStdGen`)
**Priority**: Medium
**Command (proposed)**: `-r DEVICE`, `--random DEVICE`
**Description**: Specify random number generator device
**Options**:
- `/dev/urandom` (fast, default)
- `/dev/random` (higher quality, slower)
- Custom entropy sources

**Implementation notes**:
- Haskell can read from device files
- Need to handle blocking behavior of `/dev/random`

---

### 5. Integration Features

#### 5.1 Clipboard Integration
**Status**: ✗ Not implemented
**Priority**: **High** (core planned feature)
**Command (proposed)**: `--clipboard`, `-x`
**Description**: Copy generated passphrase directly to system clipboard
**Implementation options**:
- Use `xclip` (X11)
- Use `wl-copy` (Wayland)
- Use `pbcopy` (macOS, for cross-platform support)
- Detect available clipboard tool automatically

**Example**:
```bash
correct-unicorn -w 5 --clipboard
# Output: phrase opposite cloth statement unicorn
# (also copied to clipboard)
```

#### 5.2 Pass (password-store) Integration
**Status**: ✗ Not implemented
**Priority**: **High** (core planned feature)
**Command (proposed)**: `--pass PATH`, `-P PATH`
**Description**: Generate passphrase and store in pass password manager
**Implementation**:
- Execute `pass insert PATH` with generated passphrase
- Support for `--no-echo` mode
- Optional: generate and add metadata (URL, username)

**Example**:
```bash
correct-unicorn -w 6 --pass github.com/myaccount
# Generates passphrase and executes:
# echo "phrase opposite cloth statement unicorn amazing" | pass insert github.com/myaccount
```

**Configuration (proposed)**:
- Default pass path prefix via config file
- `~/.config/correct-unicorn/config.yaml`:
  ```yaml
  pass:
    default_prefix: "websites/"
  ```

#### 5.3 Configuration File Support
**Status**: ✗ Not implemented
**Priority**: Medium
**Location (proposed)**: `~/.config/correct-unicorn/config.yaml`
**Description**: Store user preferences
**Example config**:
```yaml
# Default generation settings
defaults:
  words: 5
  separator: "-"
  wordlists:
    - "common-english"
    - "computers"

# Colorscheme for retro hacker aesthetic
colors:
  enabled: true
  scheme: "retro"
  colors: ["green", "yellow", "cyan", "magenta"]

# Randomness
random:
  source: "/dev/urandom"

# Pass integration
pass:
  default_prefix: "websites/"

# Wordlist directories
wordlists:
  directories:
    - "/usr/share/correct-unicorn"
    - "/usr/share/dict"
    - "~/.local/share/correct-unicorn/wordlists"
```

---

### 6. Terminal UI & Aesthetics

#### 6.1 Retro Hacker Aesthetics
**Status**: ✗ Not implemented
**Priority**: **High** (core design goal)
**Description**: Terminal UI with classic "retro hacker" visual style
**Design philosophy**: Should look good in all terminals, with coolretroterm as the aesthetic vibe example

**Features**:
- Traditional terminal color palette (bright greens, yellows, cyans, magentas)
- High contrast for readability across different terminals
- Clean, minimalist output design
- Optional ASCII art elements
- Works well in both modern terminals and retro-styled ones like coolretroterm

**Color scheme considerations**:
- Avoid 256-color or truecolor dependencies (stick to 16-color ANSI)
- Classic green/amber terminal colors as defaults
- High contrast combinations
- Configurable schemes for different terminal preferences

#### 6.2 Pipe-Friendly Output
**Status**: Partial (outputs to stdout)
**Priority**: High
**Enhancement**: Detect TTY vs pipe automatically
**Behavior**:
- TTY: Full color output with formatting
- Pipe/redirect: Plain text, no ANSI codes (unless `-f/--force-color`)

**Implementation**:
```haskell
import System.Posix.IO (queryTerminal, stdOutput)

main :: IO ()
main = do
  isTTY <- queryTerminal stdOutput
  -- Use colors only if isTTY or --force-color
```

#### 6.3 Progress Indicators
**Status**: ✗ Not implemented
**Priority**: Low
**Use case**: When generating many passphrases or using slow RNG
**Style**: Retro ASCII spinner or progress bar

---

### 7. Utility Features

#### 7.1 Version Information
**Status**: ✗ Not implemented
**Priority**: Low
**Command (proposed)**: `--version`
**Output**: `correct-unicorn 0.1.0.0`

#### 7.2 Verbose/Debug Mode
**Status**: ✗ Not implemented
**Priority**: Low
**Command (proposed)**: `-v`, `--verbose`
**Description**: Show generation details
**Output example**:
```
Wordlist: /usr/share/dict/english (45402 words)
Words requested: 4
Random indices: [12045, 33821, 8934, 29012]
Selected words: phrase, opposite, cloth, statement
Separator: (space)
Total length: 29 characters
```

#### 7.3 Stdin Wordlist
**Status**: ✗ Not implemented
**Priority**: Low
**Command (proposed)**: `--stdin`
**Description**: Read wordlist from stdin
**Use case**: Custom filtering, dynamic wordlists
**Example**:
```bash
cat custom-words.txt | correct-unicorn --stdin -w 4
```

---

## Implementation Roadmap

### Phase 1: Core Configurability (High Priority)
**Goal**: Make correct-unicorn configurable and production-ready

1. **Fix bold formatting bug** (app/PrettyAnsi.hs:22)
   - Change `ansiBold = "\x1b[0m"` to `ansiBold = "\x1b[1m"`

2. **Multiple wordlist support**
   - Add `-l/--wordlist` flag
   - Support absolute and relative paths
   - Default to `/usr/share/correct-unicorn/` directory
   - Ship with curated wordlists

3. **Character count minimum** (`-c/--chars`)
   - Keep generating until minimum length met
   - Display warning if impossible with wordlist

4. **Custom separators** (`-s/--separator`)
   - Default to space, support any string
   - Empty string for concatenation

5. **No-color mode** (`-p/--no-color`)
   - Add flag to disable ANSI codes
   - Auto-detect pipe/redirect

6. **Security statistics** (`-z/--security`)
   - Calculate and display entropy
   - Show crack time estimates
   - Educational output about passphrase strength

### Phase 2: Integration Features (High Priority)
**Goal**: Integrate with Linux terminal ecosystem

7. **Clipboard support** (`--clipboard`)
   - Auto-detect clipboard tool (xclip/wl-copy/pbcopy)
   - Copy to clipboard after generation
   - Optional: clipboard-only mode (no stdout)

8. **Pass integration** (`--pass PATH`)
   - Generate and insert into password-store
   - Support pass options (multiline, etc.)
   - Handle pass errors gracefully

9. **Configuration file**
   - YAML-based config in `~/.config/correct-unicorn/`
   - Store user preferences
   - Wordlist directories
   - Default generation options
   - Color schemes

### Phase 3: Visual Polish (Medium Priority)
**Goal**: Perfect the retro hacker aesthetics

10. **Retro hacker visual design**
    - Design color schemes for classic terminal aesthetic
    - Test across various terminal emulators
    - Ensure compatibility with both modern and retro-styled terminals
    - Optional ASCII art banner
    - Configurable color sets

11. **Enable additional colors**
    - Uncomment blue/magenta in usedColors
    - Make color selection configurable
    - Per-wordlist color schemes

12. **TTY detection**
    - Auto-disable colors when piping
    - Force-color flag for overriding

### Phase 4: Enhanced Generation (Medium Priority)
**Goal**: Advanced passphrase generation options

13. **Multiple passphrase generation**
    - `correct-unicorn 5` generates 5 passphrases
    - Numbered or bulleted output

14. **Force word inclusion** (`-i/--include`)
    - Ensure specific words appear
    - Validation of included words

15. **CamelCase mode** (`-u/--camelcase`)
    - Capitalize first letter of each word
    - Combine with custom separators

16. **Configurable RNG source** (`-r/--random`)
    - Support /dev/random, /dev/urandom
    - Custom entropy sources

### Phase 5: Utilities & Polish (Low Priority)
**Goal**: Complete feature parity and beyond

17. **Wordlist management commands**
    - `--wordlists`: List available
    - `--wordcount`: Show word counts
    - `--wordlists --full`: Show paths

18. **Version and info commands**
    - `--version`
    - License information
    - Help improvements

19. **Verbose mode** (`-v/--verbose`)
    - Show generation details
    - Debug information

20. **Advanced features**
    - Stdin wordlist support
    - Custom word filters
    - Pattern-based generation

---

## Design Decisions

### Haskell-Specific Advantages
- **Type safety**: Configuration parsing benefits from strong types
- **Functional composition**: Easy to compose generation functions
- **Pure functions**: Passphrase generation logic can be pure, RNG side-effectful
- **Lazy evaluation**: Can handle large wordlists efficiently

### Differences from Java/C Implementations
- **Stack build system**: Modern Haskell tooling vs Make/Maven
- **Stronger CLI parsing**: `optparse-applicative` provides excellent UX
- **Hpack integration**: `package.yaml` generates `.cabal` automatically
- **Type-safe colors**: ANSI codes as typed values, not raw strings

### Integration Philosophy
Unlike correctpony/correcthorse which are standalone tools, correct-unicorn aims to be a **hub for password generation workflow**:
- Direct clipboard access (don't manually copy-paste)
- Pass integration (generate → store in one command)
- Config file (remember user preferences)
- Terminal-native UX with retro hacker aesthetic
- Works great in all terminals, with special attention to the classic terminal look

---

## References

- **xkcd 936**: https://xkcd.com/936/ (original inspiration)
- **correctpony**: https://github.com/maandree/correctpony (Java implementation)
- **correcthorse**: Local installation (C implementation)
- **pass**: https://www.passwordstore.org/ (password-store)
- **coolretroterm**: https://github.com/Swordfish90/cool-retro-term (aesthetic vibe example)
